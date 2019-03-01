import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Traversable
import System.IO.Unsafe (unsafePerformIO)


import TestJob (executeTest)

main :: IO ()
main = do
  putStrLn ""
  putStrLn "========== START"
  executeTest
  putStrLn "========== FINISH"





-- data JobId = JobId String
--   deriving (Eq, Ord, Show)
--
-- data OperationResult e =
--     OperationResultSuccess
--   | OperationResultError e
--   deriving (Eq, Show)
--
-- data AsyncJobActions m e = AsyncJobActions
--   { executeJob :: (JobState -> IO ()) -> m (OperationResult e)
--   , cancelJob :: m (OperationResult e)
--   }
--
-- data JobRequest m e a = JobRequest
--   { jobDescription :: Text
--   , initJob :: JobId -> m (Either e (AsyncJobActions m e))
--   }
--
-- data JobState =
--     JobExecuting
--   | JobResultsReady
--   | JobResultsReturning
--   | JobCancelling
--   | JobCancelled
--   | JobFinished
--   deriving (Eq, Show)
--
-- data JobInfo m e = JobInfo
--   { jobInfoJobId :: JobId
--   , jobInfoJobDescription :: Text
--   , jobInfoCancelJob :: m (OperationResult e)
--   , jobInfoJobState :: JobState
--   }
--
--
-- {-# NOINLINE jobInfosTVarIORef #-}
-- jobInfosTVarIORef :: IORef (Map JobId (TVar (Maybe (Either e (JobInfo m e)))))
-- jobInfosTVarIORef = unsafePerformIO $ newIORef Map.empty
--
-- executeJobRequest
--   :: Show e
--   => JobRequest IO e a -> IO (Either e (JobInfo IO e))
-- executeJobRequest JobRequest{..} = do
--   let jobId = JobId "job1" -- create Job record in DB
--
--   jobInfoTVar <- liftIO $ atomically $ newTVar Nothing
--   forkIO $ initJob jobId >>= \case
--       Left e -> do
--         atomically $ writeTVar jobInfoTVar $ Just (Left e)
--       Right AsyncJobActions{..} -> do
--         atomically $ writeTVar jobInfoTVar $ Just $
--             Right JobInfo
--               { jobInfoJobId = jobId
--               , jobInfoJobDescription = jobDescription
--               , jobInfoCancelJob = cancelJob
--               , jobInfoJobState = JobExecuting
--               }
--         jobResult <- executeJob $ \jobState -> do
--           putStrLn $ "State of Job " ++ show jobId ++ " changed to " ++ show jobState
--           atomically $ modifyTVar jobInfoTVar $ \case
--             Nothing -> error "unexpected"
--             Just (Right JobInfo{..}) -> do
--               Just $ Right JobInfo{jobInfoJobState = jobState, ..}
--           when (jobState == JobFinished) $ do
--             atomicModifyIORef' jobInfosTVarIORef $ \jobInfos ->
--               (Map.delete jobId jobInfos, ())
--
--         print jobResult
--
--   waitForResultLoop jobInfoTVar >>= \case
--     Left err -> pure $ Left err
--     Right jobInfo -> do
--       atomicModifyIORef' jobInfosTVarIORef $ \jobInfos ->
--        -- TODO(denis): we should error if it already exists
--         (Map.insert jobId jobInfoTVar jobInfos, ())
--       pure $ Right jobInfo
--   where
--     waitForResultLoop :: TVar (Maybe (Either e (JobInfo m e))) -> IO (Either e (JobInfo m e))
--     waitForResultLoop jobInfoTVar =
--       atomically $ readTVar jobInfoTVar >>= \case
--           Nothing -> retry
--           Just jobInfo -> pure jobInfo
--

-- getMeMoreResults :: JobId -> (JobResult e a -> m (OperationResult e)) -> m (OperationResult e)
-- getMeMoreResults = undefined

--
-- data TestJobError =
--     Cancelled
--   | Unexpected
--   deriving (Eq, Show)
--
-- initTestJob
--   :: (Monad m, MonadIO m)
--   => JobId -> m (Either TestJobError (AsyncJobActions m TestJobError))
-- initTestJob jobId = do
--   cancellationFlag <- liftIO $ atomically $ newTVar False
--   pure $ Right AsyncJobActions
--     { executeJob = executeTestJob jobId cancellationFlag
--     , cancelJob = cancelTestJob jobId cancellationFlag
--     }
--
-- executeTestJob
--   :: (Monad m, MonadIO m)
--   => JobId -> TVar Bool -> (JobState -> IO ()) -> m (OperationResult TestJobError)
-- executeTestJob jobId cancellationFlag modifyState = do
--   results <- mapM
--       (\i -> do
--         liftIO $ threadDelay 100000
--         cancellationRequested <- liftIO $ atomically $ readTVar cancellationFlag
--         if cancellationRequested
--           then do
--             liftIO $ modifyState JobCancelled
--             pure $ OperationResultError Cancelled
--           else do
--             liftIO $ putStrLn $ "Job " ++ show jobId ++ ", Step " ++ show i
--             pure OperationResultSuccess
--       ) [1..3]
--
--   liftIO $ modifyState JobFinished
--   pure $ case filter (/= OperationResultSuccess) results of
--     [] -> OperationResultSuccess
--     (r:_) -> r
--
--
-- cancelTestJob :: (Monad m, MonadIO m) => JobId -> TVar Bool -> m (OperationResult e)
-- cancelTestJob jobId cancellationFlag = do
--   liftIO $ putStrLn $ "Cancelling job " ++ show jobId
--   liftIO $ atomically $ writeTVar cancellationFlag True
--   pure OperationResultSuccess
--
--
-- main :: IO ()
-- main = do
--   putStrLn ""
--   putStrLn "START"
--
--   let jobRequest = JobRequest
--         { jobDescription = "My Test Job"
--         , initJob = initTestJob
--         }
--
--   executeJobRequest jobRequest >>= \case
--     Left e -> putStrLn $ "Error: " ++ show e
--     Right JobInfo{..} -> do
--       putStrLn $ "JobId: " ++ show jobInfoJobId
--
--       threadDelay 150000
--       printJobInfos
--
--       cancellationResult <- jobInfoCancelJob
--       putStrLn $ "Cancellation result of job " ++ show jobInfoJobId ++ ": " ++ show cancellationResult
--
--       threadDelay 100000
--       printJobInfos
--       threadDelay 110000
--       printJobInfos
--
--
--   putStrLn "FINISH"
--   putStrLn "========================="
--   executeTest
--   putStrLn "========================="
--
--   where
--     printJobInfos = do
--       jobInfos <- readIORef jobInfosTVarIORef
--       let jobInfoTVars = Map.elems jobInfos
--       putStrLn $ "JobInfos (" ++ show (length jobInfoTVars) ++ "):"
--       forM_ jobInfoTVars $ \jobInfoTVar -> do
--         jobInfo <- atomically $ readTVar jobInfoTVar
--         putStrLn $ "  " ++ showJobInfo jobInfo
--
--     showJobInfo = \case
--       Nothing -> "[No JobInfo]"
--       Just jobInfoResult -> case jobInfoResult of
--         Left err -> "[Error]"
--         Right JobInfo{..} -> "[" ++ show jobInfoJobId ++ "," ++ show jobInfoJobState ++ "]"
--
--     waitForResultLoop jobInfoTVar =
--       atomically $ readTVar jobInfoTVar >>= \case
--         Nothing -> retry
--         Just result -> pure result
