module Data.Mail where

import Protolude

type MailTo = Text
type MailFrom = Text

data Mail = Mail {
    mailTo :: MailTo
  , mail :: ByteString
  } deriving (Show)
