{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Mail where

import Protolude

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TChan (tryReadTChan)
import GHC.Conc (atomically)

import SharedEnv


type MailM = ReaderT SharedEnv IO

threadMail :: MailM ()
threadMail = do
  sharedEnv <- ask

  maybeMail <- liftIO $ atomically $ tryReadTChan (mailTChan sharedEnv)
  case maybeMail of
    Just _ -> putStrLn $ ("I have an email ?!" :: Text)
    Nothing -> putStrLn $ ("… nothing" :: Text)

  liftIO $ threadDelay (4000000)
  putStrLn $ ("thread mail running…" :: Text)
  threadMail
  return ()
