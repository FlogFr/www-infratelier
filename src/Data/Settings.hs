module Data.Settings where


import Protolude
import Control.Monad
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Y

data Settings = Settings
  { baseUrl :: T.Text
  , production :: Bool
  , email_debug :: Bool
  , app_port :: Int
  , template_default :: HM.HashMap Text Text
  , monitoring_port :: Int
  } deriving (Show, Generic, Eq)

instance Y.FromJSON Settings

loadSettings :: T.Text -> IO (Settings)
loadSettings pathFileCfg = do
  maybeSettings <- Y.decodeFileEither . T.unpack $ pathFileCfg
  case maybeSettings of
    Right config -> return config
    Left err -> fail ("impossible to load the config.yml file" ++ show err)
