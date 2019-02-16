module Main where

import Protolude

import GHC.IO.Encoding
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TChan as TC
import Data.HashMap.Strict as HM
import System.Environment
import Data.Text (pack)
import Data.List ((!!))
import SharedEnv
import SynchronizeDatabase
import Monitoring
import MonitoringApp
import Mail
import Server

main :: IO ()
main = do
  setLocaleEncoding utf8
  
  args <- getArgs
  let configFilePath = if length args > 1
                          then args!!0
                          else "config.yml"
  let databaseFilePath = if length args > 1
                          then args!!1
                          else "database.json"

  -- Setup the STM variables
  newTChanMail <- atomically $ TC.newTChan
  newTMonitoringMetrics <- atomically $ (newTMVar $ initMonitoringMetrics)
  newTCache <- atomically $ (newTMVar HM.empty)
  newTCacheTemplate <- atomically $ (newTMVar HM.empty)

  -- New shared environment
  sharedEnv <- initSharedEnv (pack configFilePath) (pack databaseFilePath) newTChanMail newTMonitoringMetrics newTCache newTCacheTemplate

  -- Launch the three threads:
  -- - email thread
  -- - http thread
  -- - monitoring thread
  withAsync ( runReaderT threadSynchronizeDatabase sharedEnv ) $ \asyncThreadSynchronizeDatabase -> do
    threadDelay (500000)
    withAsync ( runReaderT threadMail sharedEnv ) $ \asyncThreadMail -> do
      withAsync ( startMonitoring sharedEnv ) $ \asyncThreadMonitoring -> do
        withAsync ( startApp sharedEnv ) $ \asyncThreadApp -> do

          _ <- waitAny [asyncThreadSynchronizeDatabase, asyncThreadMail, asyncThreadMonitoring, asyncThreadApp]

          return ()
