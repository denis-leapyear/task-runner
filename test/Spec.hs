{-# LANGUAGE TypeApplications #-}

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
import qualified Data.Text as Text
import Data.Traversable
import System.IO.Unsafe (unsafePerformIO)

import Lib
import TestJob (executeTest)

class MyTypeClass t where
  someFunction :: t -> Int
  empty :: t

instance MyTypeClass String where
  someFunction = length
  empty = "empty"

instance MyTypeClass Text where
  someFunction = Text.length
  empty = "text"

genericFunc :: (MyTypeClass t) => Int -> t
genericFunc n = empty

callGenericFunc :: Int
callGenericFunc =
  let mt = genericFunc @Text 42
  in someFunction mt


main :: IO ()
main = do
  putStrLn ""
  putStrLn "========== START"
--   executeTest
  someFunc
  putStrLn "========== FINISH"
  putStrLn $ "" ++ show callGenericFunc
