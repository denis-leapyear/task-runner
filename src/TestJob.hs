module TestJob where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)

import JobRunner

import JobRunnerImpl (printJobInfos)


executeTest :: IO ()
executeTest = do
  threadId <- myThreadId

  cancellationFlag <- liftIO $ atomically $ newTVar False
  jobId <- startJob initTestJob
  putStrLn $ show threadId ++ " | startJob called"

  threadDelay 150000
  printJobInfos "After 150 | "

  setResultsConsumer jobId $ \result -> do
    threadId <- myThreadId
    putStrLn $ show threadId ++ " | RESULT: " ++ result
    pure Processed

  threadDelay 100000
  printJobInfos "After 250 | "

  cancelJob jobId
  putStrLn $ show threadId ++ " | CANCELLED"

  threadDelay 100000
  printJobInfos "After 350 | "


data TestJobError =
    Cancelled
  | Unexpected
  deriving (Eq, Show)


initTestJob
  :: (Monad m, MonadIO m)
  => JobId -> m (JobDefinition m TestJobError String)
initTestJob jobId = do
  threadId <- liftIO $ myThreadId
  liftIO $ putStrLn $ show threadId ++ " | Job initiation"

  cancellationFlag <- liftIO $ atomically $ newTVar False
  pure $ JobDefinition
    { jobDefinitionDescription = "My Test Job"
    , jobDefinitionExecutionAction = executeTestJob jobId cancellationFlag
    , jobDefinitionCancellationAction = cancelTestJob jobId cancellationFlag
    }


executeTestJob
  :: (Monad m, MonadIO m)
  => JobId -> TVar Bool -> JobOps m TestJobError String -> m ()
executeTestJob jobId cancellationFlag JobOps{..} = do
    threadId <- liftIO $ myThreadId
    mapM_
      (\i -> do
        cancellationRequested <- liftIO $ atomically $ readTVar cancellationFlag
        if cancellationRequested
          then do
            setJobState JobCancelled
          else do
            liftIO $ threadDelay 100000
            liftIO $ putStrLn $ show threadId ++ " | Job " ++ show jobId ++ ", Step " ++ show i
            getResultsConsumer >>= \case
              Nothing -> pure ()
              Just consumeResults -> void $ consumeResults $ "Results from step " ++ show i
      ) [1..5]


cancelTestJob :: (Monad m, MonadIO m) => JobId -> TVar Bool -> m ()
cancelTestJob jobId cancellationFlag = liftIO $ atomically $ writeTVar cancellationFlag True
