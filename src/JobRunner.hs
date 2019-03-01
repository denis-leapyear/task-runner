module JobRunner where

import Control.Monad
import Data.Text (Text)


data JobState =
    JobExecuting
  | JobResultsReady
  | JobResultsReturning
  | JobCancelling
  | JobCancelled
  | JobFailed
  | JobFinished
  deriving (Eq, Show)


data JobId = JobId String
  deriving (Eq, Ord, Show)


data ConsumingResult err = Processed | Enough | Failed err

data JobOps m err result = JobOps
  { getResultsConsumer :: m (Maybe (result -> m (ConsumingResult err)))
  , setJobState :: JobState -> m ()
  , setJobStateToFailed :: err -> m ()
  }

data JobDefinition m err result = JobDefinition
  { jobDefinitionDescription :: Text
  , jobDefinitionExecutionAction :: JobOps m err result -> m ()
  , jobDefinitionCancellationAction :: m ()
  }


class Monad m => JobExecutor m where

  startJob
    :: Show err
    => (JobId -> m (JobDefinition m err result))
    -> m JobId

  cancelJob :: JobId -> m ()

  setResultsConsumer :: JobId -> (result -> m (ConsumingResult err)) -> m ()
