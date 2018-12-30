module SharedEnv where

import Protolude

import qualified Data.Text                     as T
import qualified Control.Concurrent.STM.TMVar  as STMVar
import Data.HashMap.Strict as HM
import Text.Mustache.Types
import Data


data SharedEnv = SharedEnv
  { settings :: Settings
  , cache :: STMVar.TMVar (HashMap Text ByteString)
  , cacheTemplate :: STMVar.TMVar (HashMap FilePath Template)
  } deriving (Generic)


initSharedEnv :: T.Text -> STMVar.TMVar (HashMap Text ByteString) -> STMVar.TMVar TemplateCache -> IO (SharedEnv)
initSharedEnv pathFileCfg tCache tCachetemplate = do
  settingsLoaded <- loadSettings pathFileCfg

  return (SharedEnv settingsLoaded tCache tCachetemplate)
