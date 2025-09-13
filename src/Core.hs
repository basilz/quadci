module Core where

import Data.List (all, find)
import qualified Data.Time.Clock.POSIX as Time
import Docker
  ( ContainerExitCode (..),
    ContainerId,
    ContainerStatus (..),
    CreateContainerOptions (..),
    FetchLogOptions (..),
    Image,
    Service,
    Volume,
    createContainer,
    exitCodeToInt,
    startContainer,
  )
import RIO
  ( Applicative (pure),
    ByteString,
    Either (..),
    Eq ((==)),
    IO,
    Map,
    Maybe (Just, Nothing),
    NonEmpty,
    Ord,
    Show,
    Text,
    all,
    concat,
    not,
    undefined,
    ($),
    (<&>),
    (<>), Generic, Int,
  )
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.Text as Text
import qualified Data.Aeson as Aeson
import qualified Codec.Serialise as Serialise


newtype StepName = StepName Text
  deriving (Eq, Show, Ord, Generic, Aeson.FromJSON, Serialise.Serialise)

data Step = Step
  { name :: StepName,
    commands :: NonEmpty Text,
    image :: Docker.Image
  }
  deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)

newtype Pipeline = Pipeline
  { steps :: NonEmpty Step
  }
  deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)

data StepResult = StepFailed Docker.ContainerExitCode | StepSucceeded deriving (Eq, Show, Generic, Serialise.Serialise)

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exitCode = if Docker.exitCodeToInt exitCode == 0 then StepSucceeded else StepFailed exitCode

data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult,
    volume :: Docker.Volume
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show, Generic, Serialise.Serialise)

data BuildRunningState = BuildRunningState
  { step :: StepName,
    container :: Docker.ContainerId
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  | BuildUnexpectedState Text
  deriving (Eq, Show, Generic, Serialise.Serialise)

type LogCollection = Map StepName CollectionStatus

data CollectionStatus = CollectionReady | CollectingLogs Docker.ContainerId Time.POSIXTime | CollectionFinished deriving (Eq, Show)

data Log = Log {output :: ByteString, step :: StepName} deriving (Eq, Show, Ord, Generic, Serialise.Serialise)

newtype BuildNumber = BuildNumber Int deriving (Eq, Show, Generic, Serialise.Serialise, Ord)

buildNumberToInt :: BuildNumber -> Int
buildNumberToInt (BuildNumber n) = n

progress :: Docker.Service -> Build -> IO Build
progress docker build = case build.state of
  BuildReady -> case buildHasNextStep build of
    Left result ->
      pure build {state = BuildFinished result}
    Right step -> do
      let script =
            Text.unlines $
              ["set -ex"] <> NonEmpty.toList step.commands
      let options =
            Docker.CreateContainerOptions
              { image = step.image,
                script = script,
                volume = build.volume
              }
      docker.pullImage step.image
      container <- docker.createContainer options
      docker.startContainer container
      let s =
            BuildRunningState
              { step = step.name,
                container = container
              }
      pure build{state = BuildRunning s}
  BuildRunning state -> do
    status <- docker.containerStatus state.container

    case status of
      ContainerRunning ->
        pure build
      ContainerExited exit -> do
        let result = exitCodeToStepResult exit
        pure
          build
            { state = BuildReady,
              completedSteps = M.insert state.step result build.completedSteps
            }
      ContainerOther other -> do
        let s = BuildUnexpectedState other
        pure build {state = BuildFinished s}
  BuildFinished _ -> pure build

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if allSucceded
    then case nextStep of
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
  where
    allSucceded = all (StepSucceeded ==) build.completedSteps
    nextStep = find f build.pipeline.steps
    f step = not $ M.member step.name build.completedSteps

collectLogs :: Docker.Service -> LogCollection -> Build -> IO (LogCollection, [Log])
collectLogs docker collection build = do
  now <- Time.getPOSIXTime
  logs <- runCollection docker now collection
  let newCollection = updateCollection build.state now collection
  pure (newCollection, logs)

initLogCollection :: Pipeline -> LogCollection
initLogCollection pipeline =
  M.fromList $ NonEmpty.toList steps
  where
    steps = pipeline.steps <&> \step -> (step.name, CollectionReady)

updateCollection :: BuildState -> Time.POSIXTime -> LogCollection -> LogCollection
updateCollection state lastCollection collection = M.mapWithKey f collection
  where
    f step = \case
      CollectionReady ->
        case state of
          BuildRunning state ->
            if state.step == step
              then CollectingLogs state.container 0
              else CollectionReady
          _ -> CollectionReady
      CollectingLogs _ _ ->
        case state of
          BuildRunning state ->
            if state.step == step
              then CollectingLogs state.container lastCollection
              else CollectionFinished
          _ -> CollectionFinished
      CollectionFinished -> CollectionFinished

runCollection :: Docker.Service -> Time.POSIXTime -> LogCollection -> IO [Log]
runCollection docker collectUntil collection = do
  logs <- M.traverseWithKey f collection
  pure $ concat (M.elems logs)
  where
    f step = \case
      CollectionReady -> pure []
      CollectionFinished -> pure []
      CollectingLogs container since -> do
        let options =
              Docker.FetchLogOptions {container = container, since = since, until = collectUntil}
        output <- docker.fetchLogs options
        pure [Log {step = step, output = output}]