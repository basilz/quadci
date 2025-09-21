module JobHandler.Memory where

import qualified Agent
import qualified Control.Concurrent.STM as STM
import Core
import Core (Build (pipeline))
import Data.Foldable (find)
import Docker (createService)
import qualified JobHandler
import RIO
import RIO.HashMap (update)
import qualified RIO.List as List
import qualified RIO.Map as Map

data State = State
  { jobs :: Map BuildNumber JobHandler.Job,
    logs :: Map (BuildNumber, StepName) ByteString,
    nextBuildNumber :: Int
  }
  deriving (Eq, Show)

createService :: IO JobHandler.Service
createService = do
  state <- STM.newTVarIO State {jobs = mempty, logs = mempty, nextBuildNumber = 1}
  pure
    JobHandler.Service
      { queueJob = \pipeline -> STM.atomically do
          STM.stateTVar state (queueJob_ pipeline),
        findJob = \number -> STM.atomically do
          s <- STM.readTVar state
          pure (findJob_ number s),
        dispatchCmd = STM.atomically do
          STM.stateTVar state dispatchCmd_,
        processMsg = \msg -> STM.atomically do
          STM.modifyTVar' state (processMsg_ msg)
      }

queueJob_ :: Pipeline -> State -> (BuildNumber, State)
queueJob_ pipeline state =
  (number, updatedState)
  where
    number = BuildNumber state.nextBuildNumber
    job =
      JobHandler.Job
        { pipeline = pipeline,
          state = JobHandler.JobQueued
        }
    updatedState =
      state
        { jobs = Map.insert number job state.jobs,
          nextBuildNumber = state.nextBuildNumber + 1
        }

findJob_ :: BuildNumber -> State -> Maybe JobHandler.Job
findJob_ number state = Map.lookup number state.jobs

dispatchCmd_ :: State -> (Maybe Agent.Cmd, State)
dispatchCmd_ state =
  case List.find isQueued (Map.toList state.jobs) of
    Just (number, job) ->
      let cmd = Just $ Agent.StartBuild number job.pipeline
          updatedJob = job{state = JobHandler.JobAssigned}
          updatedState = Map.insert number updatedJob state.jobs
       in (cmd, state {jobs = updatedState})
    _ -> (Nothing, state)
  where
    isQueued :: (BuildNumber, JobHandler.Job) -> Bool
    isQueued (_, job) = job.state == JobHandler.JobQueued


processMsg_ :: Agent.Msg -> State -> State
processMsg_ msg state = case msg of 
  Agent.LogCollected number log ->
    let updatedLogs = Map.insertWith (flip mappend) (number, log.step) log.output state.logs
    in state
  Agent.BuildUpdated number build ->
    let f job = job{state = JobHandler.JobScheduled build}
    in state{jobs = Map.adjust f number state.jobs}
