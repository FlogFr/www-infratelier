module Data.NewsletterSubscriber where

import Protolude

import Data.Aeson
import System.Random
import Data.UUID
import Data.Time.Clock

data NewsletterSubscriber = NewsletterSubscriber {
    newsletterSubscriberId          :: UUID
  , newsletterSubscriberCreatedAt   :: UTCTime
  , newsletterSubscriberEmail       :: Text
  , newsletterSubscriberIP          :: Text
  } deriving (Generic, Eq, Show, Ord)

instance FromJSON NewsletterSubscriber
instance ToJSON NewsletterSubscriber

newNewsletterSubscriber :: Text -> Text -> IO NewsletterSubscriber
newNewsletterSubscriber email' ip' = do
  uuid' <- randomIO :: IO UUID
  currentTime' <- getCurrentTime
  pure $ NewsletterSubscriber uuid' currentTime' email' ip'
