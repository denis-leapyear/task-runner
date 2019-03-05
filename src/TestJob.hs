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

  jobId <- startJob initTestJob
  initialJobState <- getJobState jobId
  putStrLn $ show threadId ++ " | startJob called " ++ show initialJobState

  threadDelay 100000
  printJobInfos ""

  setResultsConsumer jobId $ Just $ \result -> do
    threadId <- myThreadId
    putStrLn $ show threadId ++ " | RESULT1: " ++ result
    pure Processed

  threadDelay 50000
  printJobInfos ""

  setResultsConsumer jobId Nothing

  threadDelay 1000000
  printJobInfos "Long wait | "

  setResultsConsumer jobId $ Just $ \result -> do
    threadId <- myThreadId
    putStrLn $ show threadId ++ " | RESULT2: " ++ result
    pure Processed

--   setResultsConsumer jobId (Just consumeResult)


  threadDelay 100000

  cancelJob jobId
  putStrLn $ show threadId ++ " | CANCELLED"

  threadDelay 100000
  printJobInfos "End | "


consumeResult :: (Monad m, MonadIO m) => Int -> m (ConsumingResult err)
consumeResult result = do
    threadId <- liftIO $ myThreadId
    liftIO $ putStrLn $ show threadId ++ " | RESULT22: " ++ show result
    pure Processed


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
            setJobState JobResultsReady
            getResultsConsumer >>= \case
              Nothing -> do
                liftIO $ putStrLn $ show threadId ++ " | Job " ++ show jobId ++ " has no consumer"
                pure ()
              Just consumeResults -> do
                setJobState JobResultsReturning
                void $ consumeResults $ "Results from step " ++ show i
            setJobState JobExecuting
      ) [1..5]


cancelTestJob :: (Monad m, MonadIO m) => JobId -> TVar Bool -> m ()
cancelTestJob jobId cancellationFlag = liftIO $ atomically $ writeTVar cancellationFlag True
