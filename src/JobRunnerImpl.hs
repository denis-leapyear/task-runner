module JobRunnerImpl where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (forM_, when)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import JobRunner

data JobInfo m err result = JobInfo
  { jobInfoJobId :: JobId
  , jobInfoJobDescription :: Text
  , jobInfoCancellationAction :: m ()
  , jobInfoJobState :: JobState
  , jobInfoError :: Maybe err
  , jobInfoResultsConsumer :: Maybe (result -> m (ConsumingResult err))
  }


{-# NOINLINE jobMapIORef #-}
jobMapIORef :: IORef (Map JobId (TVar (JobInfo m err result)))
jobMapIORef = unsafePerformIO $ newIORef Map.empty


-- TODO(denis): we should error if it already exists
registerJobInfo :: TVar (JobInfo m err result) -> IO ()
registerJobInfo jobInfoTVar = do
  jobId <- jobInfoJobId <$> (atomically $ readTVar jobInfoTVar)
  threadId <- myThreadId
  putStrLn $ show threadId ++ " | Registering job " ++ show jobId
  atomicModifyIORef' jobMapIORef $ \jobInfos -> (Map.insert jobId jobInfoTVar jobInfos, ())


unregisterJobInfo :: JobId -> IO ()
unregisterJobInfo jobId = do
  threadId <- myThreadId
  putStrLn $ show threadId ++ " | Unregistering job " ++ show jobId
  atomicModifyIORef' jobMapIORef $ \jobInfos -> (Map.delete jobId jobInfos, ())


updateJobState :: JobId -> JobState -> IO ()
updateJobState jobId jobState = do
  threadId <- myThreadId
  jobInfos <- readIORef jobMapIORef
  case (Map.lookup jobId jobInfos) of
    Nothing -> error "cannot find JobInfo"
    Just jobInfoTVar -> do
      atomically $ modifyTVar jobInfoTVar $ \JobInfo{..} -> JobInfo{jobInfoJobState = jobState, ..}
      putStrLn $ show threadId ++ " | Job " ++ show jobId ++ " new state " ++ show jobState
      when (jobState == JobFinished) $ do
        unregisterJobInfo jobId


failJob :: Show err => JobId -> err -> IO ()
failJob jobId jobError = do
  jobInfos <- readIORef jobMapIORef
  case (Map.lookup jobId jobInfos) of
    Nothing -> error "cannot find JobInfo"
    Just jobInfoTVar ->
      atomically $ modifyTVar jobInfoTVar $ \JobInfo{..} ->
        JobInfo{jobInfoJobState = JobFailed, jobInfoError = Just jobError, ..}


getJobInfo :: JobId -> IO (JobInfo m err result)
getJobInfo jobId = do
  jobInfos <- readIORef jobMapIORef
  case (Map.lookup jobId jobInfos) of
    Nothing -> error "cannot find JobInfo"
    Just jobInfoTVar -> atomically $ readTVar jobInfoTVar

instance JobExecutor IO where
  startJob = startJobImpl
  cancelJob = cancelJobImpl
  setResultsConsumer = setResultsConsumerImpl

startJobImpl
  :: (m ~ IO, Show err)
  => (JobId -> m (JobDefinition m err result))
  -> m JobId
startJobImpl getJobDefinition = do
  let jobId = JobId "job1" -- create Job record in DB
  JobDefinition{..} <- getJobDefinition jobId
  let jobInfo = JobInfo
        { jobInfoJobId = jobId
        , jobInfoJobDescription = jobDefinitionDescription
        , jobInfoCancellationAction = jobDefinitionCancellationAction
        , jobInfoJobState = JobExecuting
        , jobInfoError = Nothing
        , jobInfoResultsConsumer = Nothing
        }

  jobInfoTVar <- atomically $ newTVar jobInfo
  registerJobInfo jobInfoTVar
  printJobInfos "After registration | "

  forkIO $ do
    executionResult <- jobDefinitionExecutionAction
      JobOps
        { getResultsConsumer = do
            JobInfo{..} <- getJobInfo jobId
            pure jobInfoResultsConsumer
        , setJobState = updateJobState jobId
        , setJobStateToFailed = failJob jobId
        }
    updateJobState jobId JobFinished

  pure jobId


cancelJobImpl :: JobId -> IO ()
cancelJobImpl jobId = do
  jobInfos <- readIORef jobMapIORef
  case (Map.lookup jobId jobInfos) of
    Nothing -> error $ "Cannot find JobInfo by jobId: " ++ show jobId
    Just jobInfoTVar -> do
      JobInfo{..} <- atomically $ readTVar jobInfoTVar
      jobInfoCancellationAction


setResultsConsumerImpl :: JobId -> (result -> m (ConsumingResult err)) -> IO ()
setResultsConsumerImpl jobId resultsConsumer = do
  jobInfos <- readIORef jobMapIORef
  case (Map.lookup jobId jobInfos) of
    Nothing -> error "cannot find JobInfo"
    Just jobInfoTVar -> do
      atomically $ modifyTVar jobInfoTVar $ \JobInfo{..} ->
        JobInfo{jobInfoResultsConsumer = Just resultsConsumer, ..}


-- Helpers
printJobInfos :: String -> IO ()
printJobInfos prefix = do
  threadId <- myThreadId

  jobInfoTVars <- Map.elems <$> readIORef jobMapIORef
  putStrLn $ show threadId ++ " | " ++ prefix ++ "JobInfos (" ++ show (length jobInfoTVars) ++ "):"
  forM_ jobInfoTVars $ \jobInfoTVar -> do
    jobInfo <- atomically $ readTVar jobInfoTVar
    putStrLn $ show threadId ++ " |   " ++ showJobInfo jobInfo
  where
    showJobInfo JobInfo{..} = "  " ++ show jobInfoJobId ++ "," ++ show jobInfoJobState

