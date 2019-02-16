module Data.User where

import Protolude

import Data.Aeson
import Data.UUID (UUID)

data User = User {
    userIsAdmin :: Bool
  , userUuid :: UUID
  , userFirstName :: Maybe Text
  , userLastName :: Maybe Text
  , userProfilePicture :: Maybe Text
  , userName :: Text
  , userEmail :: Text
  , userSubscription :: Text
  , userNewsletter :: Bool
  } deriving (Generic, Show)

instance ToJSON User where
  toJSON (User _ id' firstName' lastName' profilePicture' name' email' subscription' newsletter') = 
    object [
        "id"     .= id'
      , "firstName"  .= firstName'
      , "lastName"  .= lastName'
      , "profilePicture"  .= profilePicture'
      , "name"  .= name'
      , "email"  .= email'
      , "subscription"  .= subscription'
      , "newsletter"  .= newsletter'
      ]
