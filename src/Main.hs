module Main where

import Protolude

import Control.Concurrent.STM
import Data.HashMap.Strict as HM
import SharedEnv
import Server


main :: IO ()
main = do
  newTCache <- atomically $ (newTMVar HM.empty)
  newTCacheTemplate <- atomically $ (newTMVar HM.empty)

  -- New shared environment
  sharedEnv <- initSharedEnv "config.yml" newTCache newTCacheTemplate

  startApp sharedEnv
