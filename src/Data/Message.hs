module Data.Message where

import Protolude

import Data.Aeson
import System.Random
import Data.UUID
import Data.Time.Clock

data Message = Message {
    messageId          :: UUID
  , messageCreatedAt   :: UTCTime
  , messageMessage     :: Text
  , messageAuthorEmail :: Text
  , messageIP          :: Text
  } deriving (Generic, Eq, Show, Ord)

instance FromJSON Message
instance ToJSON Message

newMessage :: Text -> Text -> Text -> IO Message
newMessage message' email' ip' = do
  uuid' <- randomIO :: IO UUID
  currentTime' <- getCurrentTime
  pure $ Message uuid' currentTime' message' email' ip'
