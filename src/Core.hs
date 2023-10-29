module Core where

import Data.List (all, find)
import Docker
  ( ContainerExitCode (..),
    ContainerId,
    ContainerStatus (..),
    CreateContainerOptions (..),
    Image,
    Service,
    createContainer,
    exitCodeToInt,
    startContainer, Volume
  )
import RIO
  ( Applicative (pure),
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
    not,
    ($),
    (<>)
  )
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.Text as Text

newtype StepName = StepName Text
  deriving (Eq, Show, Ord)

data Step = Step {name :: StepName, commands :: NonEmpty Text, image :: Docker.Image}
  deriving (Eq, Show)

newtype Pipeline = Pipeline {steps :: NonEmpty Step}
  deriving (Eq, Show)

data StepResult = StepFailed Docker.ContainerExitCode | StepSucceeded deriving (Eq, Show)

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exitCode = if Docker.exitCodeToInt exitCode == 0 then StepSucceeded else StepFailed exitCode

data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult,
    volume :: Docker.Volume
  }
  deriving (Eq, Show)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildRunningState = BuildRunningState
  { step :: StepName,
    container :: Docker.ContainerId
  }
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  | BuildUnexpectedState Text
  deriving (Eq, Show)

progress :: Docker.Service -> Build -> IO Build
progress docker build = case build.state of
  BuildReady -> case buildHasNextStep build of
    Left result ->
      pure build {state = BuildFinished result}
    Right step -> do
      let script = Text.unlines 
            $ ["set -ex"] <> NonEmpty.toList step.commands
      let options = 
            Docker.CreateContainerOptions 
                { image = step.image, 
                  script = script,
                  volume = build.volume 
                }
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
