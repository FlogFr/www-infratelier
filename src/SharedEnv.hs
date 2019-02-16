module SharedEnv where

import Protolude

import Data.Text (unpack)
import System.IO.Error
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text                     as T
import qualified Control.Concurrent.STM.TChan  as STMChan
import qualified Control.Concurrent.STM.TMVar  as STMVar
import Control.Concurrent.STM
import Data.HashMap.Strict as HM
import Data.Mail
import Text.Mustache.Types
import           Monitoring
import           Data.Settings


data SharedEnv = SharedEnv
  { settings :: Settings
  , mailTChan :: STMChan.TChan (Mail)
  , monitoringMetrics :: STMVar.TMVar (MonitoringMetrics)
  , cache :: STMVar.TMVar (HashMap Text ByteString)
  , cacheTemplate :: STMVar.TMVar (HashMap FilePath Template)
  , db :: STMVar.TMVar A.Value
  } deriving (Generic)


loadDatabase :: T.Text -> IO (STMVar.TMVar A.Value)
loadDatabase dbFile = do
  databaseContent <- BSL.readFile (unpack dbFile)
  let mDbLoaded = A.decode databaseContent :: Maybe A.Value
  case mDbLoaded of
    Just dbLoaded -> atomically $ (newTMVar $ dbLoaded)
    Nothing -> ioError $ userError "impossible to load the database"


initSharedEnv :: T.Text -> T.Text -> STMChan.TChan (Mail) -> STMVar.TMVar (MonitoringMetrics) -> STMVar.TMVar (HashMap Text ByteString) -> STMVar.TMVar TemplateCache -> IO (SharedEnv)
initSharedEnv pathFileCfg dbFile tChanMail tMonitoringMetrics tCache tCachetemplate = do
  settingsLoaded <- loadSettings pathFileCfg
  dbLoaded <- loadDatabase dbFile

  return (SharedEnv settingsLoaded tChanMail tMonitoringMetrics tCache tCachetemplate dbLoaded)
