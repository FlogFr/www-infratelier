module Data.API where

import Protolude
import Data.Aeson


data GeolocUser = GeolocUser {
    lat :: Double
  , lng :: Double
  } deriving (Generic, Show)

instance FromJSON GeolocUser
instance ToJSON GeolocUser
