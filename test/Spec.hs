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
