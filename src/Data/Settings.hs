module Data.Settings where


import Protolude
import Control.Monad
import qualified Data.Map.Strict as M hiding (map)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Data.BlogPostEntry

data Settings = Settings
  { base_url :: T.Text
  , production :: Bool
  , email_debug :: Bool
  , app_port :: Int
  , blog_entries :: M.Map Text (M.Map Text [BlogPostEntry])
  } deriving (Show, Generic, Eq)

instance Y.ToJSON Settings
instance Y.FromJSON Settings

loadSettings :: T.Text -> IO (Settings)
loadSettings pathFileCfg = do
  maybeSettings <- Y.decodeFileEither . T.unpack $ pathFileCfg
  case maybeSettings of
    Right config -> return config
    Left err -> fail ("impossible to load the config.yml file" ++ show err)
