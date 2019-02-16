module Data.MenuLine where

import Protolude
import Data.Aeson

import Data.UUID (UUID)


data MenuLine = MenuLine {
    menuLineID :: UUID
  , menuLineTitle :: Text
  , menuLinePrice :: Double
  } deriving (Generic, Show)

instance ToJSON MenuLine
