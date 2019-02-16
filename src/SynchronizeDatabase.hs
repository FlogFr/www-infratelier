module SynchronizeDatabase where

import Protolude

import Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import Control.Concurrent.STM.TMVar
import SharedEnv

type SynchronizeDatabaseM = ReaderT SharedEnv IO


syncDatabase :: SynchronizeDatabaseM ()
syncDatabase = do
  sharedEnv <- ask
  let stmDatabase = db sharedEnv
  database' <- liftIO $ atomically $ readTMVar stmDatabase
  _ <- liftIO $ BS.writeFile "database.json" $ encode database' 
  pure ()

threadSynchronizeDatabase :: SynchronizeDatabaseM ()
threadSynchronizeDatabase = do
  liftIO $ threadDelay (10000000)
  putStrLn $ ("thread synchronize database running…" :: Text)
  syncDatabase
  threadSynchronizeDatabase
  pure ()
