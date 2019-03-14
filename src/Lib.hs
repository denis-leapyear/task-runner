module Lib
    ( someFunc
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Typeable (Proxy(..), cast)
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection

someFunc :: IO ()
someFunc = do

  jobId <- start initJob
  putStrLn $ "Started job: " ++ show jobId
  tVar <- readIORef ref
  threadDelay 3000000

  -- set result consumer after some delay
  atomically $ modifyTVar tVar $ \case
    Nothing -> error "no job info"
    Just JobInfo{..} ->
      let realFuncTypeRep = typeOf $ Just consumeResultsImpl
          funcTypeRep = typeOf maybeResultsConsumer
      in case realFuncTypeRep `eqTypeRep` funcTypeRep of
           Nothing -> Nothing
           Just HRefl -> Just JobInfo { maybeResultsConsumer = Just consumeResultsImpl, ..}

  threadDelay 5000000


-- Job's result consumer (set in a protocol handler, like Async Select)
consumeResultsImpl :: Res -> IO ConsumingResult
consumeResultsImpl res = do
  putStrLn $ "consumeResultsImpl: " ++ show res
  pure Processed



{-# NOINLINE ref #-}
ref :: IORef (TVar (Maybe JobInfo))
ref = unsafePerformIO $ (atomically $ newTVar Nothing) >>= newIORef


type AsyncJobId = Int

data ConsumingResult = Processed | NotReady | Failed
  deriving Show

-- operations available for Job implementations
data JobOps = forall result. (Typeable result) => JobOps
  { consumeResults :: result -> IO ConsumingResult
  }

-- job definitions provided by Job implementations
data JobDef result = JobDef
  { getJobDescription     :: String
  , executeJob            :: JobOps -> IO ()
  , resultsConsumer       :: result -> IO ConsumingResult
  }

-- information about running Jobs kept by job executor
data JobInfo = forall result. (Typeable result) => JobInfo
  { jobId :: AsyncJobId
  , maybeResultsConsumer :: Maybe (result -> IO ConsumingResult)
  }

-- job executor's interface: start a job
start
  :: forall result m. (Typeable result, MonadIO m)
  => (AsyncJobId -> m (JobDef result))
  -> m AsyncJobId
start initJob = do
  let jId = 5
  jobDef@JobDef{..} <- initJob jId
  liftIO $ do

    let jobInfo = JobInfo
          { jobId = jId
          , maybeResultsConsumer = (Nothing :: Maybe (result -> IO ConsumingResult))
          }

    tVar <- readIORef ref
    atomically $ modifyTVar tVar $ \_ -> Just jobInfo

    let jobOps = JobOps { consumeResults = resultsConsumer }
    forkIO $ executeJob jobOps
    pure jId


-- =================== helper functions for Job implementers
waitingConsumeResultsImpl
  :: forall result. (Typeable result)
  => AsyncJobId -> result -> IO ConsumingResult
waitingConsumeResultsImpl jobId res = waitForResultsConsumer jobId >>= \case
  Nothing -> pure NotReady
  Just consumeResults -> consumeResults res

waitForResultsConsumer
  :: forall result. (Typeable result)
  => AsyncJobId -> IO (Maybe (result -> IO ConsumingResult))
waitForResultsConsumer jobId = do
  tVar <- readIORef ref
  putStrLn "          WAITING..."
  rc <- atomically $ do
    maybeJobInfo <- readTVar tVar
    case maybeJobInfo of
      Nothing -> pure Nothing
      Just JobInfo{..} ->
        let realFuncTypeRep = typeOf (undefined :: Maybe (result -> IO ConsumingResult))
            funcTypeRep = typeOf maybeResultsConsumer
        in case realFuncTypeRep `eqTypeRep` funcTypeRep of
             Nothing -> error "no match"
             Just HRefl -> case maybeResultsConsumer of
               Nothing -> retry
               Just resultsConsumer -> pure maybeResultsConsumer
  putStrLn "          GOT IT"
  pure rc

executeResultsConsumer
  :: forall concreteresult result. (Typeable concreteresult, Typeable result)
  => concreteresult -> (result -> IO ConsumingResult) -> IO ()
executeResultsConsumer res consumeResults = do
  let funcTypeRep = typeOf consumeResults
  let funcResultTypRep = typeOf (undefined :: IO ConsumingResult)
  let funcSigTypeRep = Fun (typeOf res) funcResultTypRep
  case funcSigTypeRep `eqTypeRep` funcTypeRep of
    Nothing -> do
      putStrLn "executeJobImpl: No Match"
    Just HRefl -> do
      consumeResults res
      putStrLn "executeJobImpl: Match"

-- =============== a Job implementation

-- a result
data Res = Res1 | Res2
  deriving Show

-- job initiation function (passed to job executor's 'start' function)
initJob :: MonadIO m => AsyncJobId -> m (JobDef Res)
initJob jobId = pure JobDef
  { getJobDescription = "testjob" ++ show jobId
  , executeJob = executeJobImpl
  , resultsConsumer = waitingConsumeResultsImpl jobId
  }

-- main Job's body
executeJobImpl :: JobOps -> IO ()
executeJobImpl jobOps@JobOps{..} = do
  threadDelay 1000
  putStrLn "executeJobImpl"
  executeResultsConsumer Res1 consumeResults
