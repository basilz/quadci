module Core where

import Data.List
import Docker (ContainerExitCode (..), CreateContainerOptions (CreateContainerOptions), Image, Service, createContainer, exitCodeToInt, startContainer)
import RIO
import qualified RIO.Map as M

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
    { pipeline :: Pipeline
    , state :: BuildState
    , completedSteps :: Map StepName StepResult
    }
    deriving (Eq, Show)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

data BuildState
    = BuildReady
    | BuildRunning BuildRunningState
    | BuildFinished BuildResult
    deriving (Eq, Show)

newtype BuildRunningState = BuildRunningState {step :: StepName} deriving (Eq, Show)

data BuildResult
    = BuildSucceeded
    | BuildFailed
    deriving (Eq, Show)

progress :: Docker.Service -> Build -> IO Build
progress docker build = case build.state of
    BuildReady -> case buildHasNextStep build of
        Left result ->
            pure build{state = BuildFinished result}
        Right step -> do
            let options = Docker.CreateContainerOptions step.image
            container <- docker.createContainer options
            docker.startContainer container
            let s = BuildRunningState{step = step.name}
            pure $ build{state = BuildRunning s}
    BuildRunning state -> do
        let exit = Docker.ContainerExitCode 0
            result = exitCodeToStepResult exit
        pure
            build
                { state = BuildReady
                , completedSteps = M.insert state.step result build.completedSteps
                }
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