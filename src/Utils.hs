module Utils where

import Protolude

import Control.Lens
import System.IO.Error
import           Text.Blaze.Html5              as H
                                                ( Html )
import qualified Data.HashMap.Strict           as HM
                                         hiding ( map )
import           Control.Monad.Logger
import qualified Text.Mustache.Types as M
import Data.Aeson
import Data.Aeson.Lens
import Control.Exception
import Control.Concurrent.STM.TMVar
import System.Random
import Data.Char (Char, chr)
import Network.HTTP.Media ((//))
import qualified Data.ByteString.Lazy          as BSL
import Data.Text as T hiding (reverse, filter)
import qualified Data.Vector as V
import Servant
import HandlerM
import SharedEnv
import Template
import Mustache
import Data


data OctetStreamFavico
instance Accept OctetStreamFavico where
  contentType _ = "image" // "x-icon"
instance MimeRender OctetStreamFavico ByteString where
  mimeRender _ = BSL.fromStrict


data OctetStreamCss
instance Accept OctetStreamCss where
  contentType _ = "text" // "css"
instance MimeRender OctetStreamCss ByteString where
  mimeRender _ = BSL.fromStrict


data OctetStreamText
instance Accept OctetStreamText where
  contentType _ = "text" // "plain"
instance MimeRender OctetStreamText Text where
  mimeRender _ = BSL.fromStrict . encodeUtf8

data XmlText
instance Accept XmlText where
  contentType _ = "application" // "xml"
instance MimeRender XmlText Text where
  mimeRender _ = BSL.fromStrict . encodeUtf8

data AtomText
instance Accept AtomText where
  contentType _ = "application" // "atom+xml"
instance MimeRender AtomText Text where
  mimeRender _ = BSL.fromStrict . encodeUtf8


generateRandomPassword :: IO Text
generateRandomPassword = do
  newPassword <- passwordString []
  return $ T.pack newPassword
  where passwordString :: [Char] -> IO [Char]
        passwordString fullPassword@[_,_,_,_,_,_,_,_] = return fullPassword
        passwordString currPassword = do
          newIntR <- getStdRandom (randomR (48, 122)) :: IO Int
          if (48 < newIntR && newIntR < 57) || (65 < newIntR && newIntR < 90) || (97 < newIntR && newIntR < 122)
            then passwordString (chr(newIntR):currPassword)
            else passwordString currPassword

getNewsletterSubscribers :: HandlerM [NewsletterSubscriber]
getNewsletterSubscribers = do
  sharedEnv <- ask
  let stmDatabase = db sharedEnv
  database' <- liftIO $ atomically $ readTMVar stmDatabase
  let mNLSubscribers = database' ^? key "NLSubscribers" . _Array
  case mNLSubscribers of
    Just nlSubscribers -> do
      res' <- sequence $ (decodeNLSubOrThrow <$> (V.toList $ nlSubscribers))
      pure $ reverse $ sortOn (newsletterSubscriberCreatedAt) res'
    Nothing -> pure []
  where
    decodeNLSubOrThrow :: Value -> HandlerM NewsletterSubscriber
    decodeNLSubOrThrow val' = do
      let eitherMessage = eitherDecodeNLSub val'
      case eitherMessage of
        Right msg -> pure msg
        Left txt' -> throw $ userError txt'

    eitherDecodeNLSub :: Value -> Either [Char] NewsletterSubscriber
    eitherDecodeNLSub val' = do
      let resultJSONParser = fromJSON val'
      case resultJSONParser of
        Error _ -> Left "Impossible to parse the message"
        Success msg' -> Right msg'

putNewsletterSubscribers :: [NewsletterSubscriber] -> HandlerM ()
putNewsletterSubscribers nlSubscribers' = do
  sharedEnv <- ask
  let stmDatabase = db sharedEnv
  database' <- liftIO $ atomically $ readTMVar stmDatabase
  let newDatabase' = database' &_Object . at "NLSubscribers" ?~ (Array $ V.fromList (toJSON <$> nlSubscribers'))
  _ <- liftIO $ atomically $ swapTMVar stmDatabase newDatabase'
  pure ()

insertEmailToNewsletterList :: NewsletterSubscriber -> HandlerM ()
insertEmailToNewsletterList sub' = do
  $(logInfo) ("log: new email to insert to the newsletter - " <> (show sub') :: Text)
  nlSubscribers <- getNewsletterSubscribers
  _ <- putNewsletterSubscribers (nlSubscribers ++ [sub'])
  pure ()

getContactMessages :: HandlerM [Message]
getContactMessages = do
  sharedEnv <- ask
  let stmDatabase = db sharedEnv
  database' <- liftIO $ atomically $ readTMVar stmDatabase
  let mContactMessages = database' ^? key "ContactMessages" . _Array
  case mContactMessages of
    Just contactMessages -> do
      res' <- sequence $ (decodeMessageOrThrow <$> (V.toList $ contactMessages))
      pure $ reverse $ sortOn (messageCreatedAt) res'
    Nothing -> pure []
  where
    decodeMessageOrThrow :: Value -> HandlerM Message
    decodeMessageOrThrow val' = do
      let eitherMessage = eitherDecodeMessageOrThrow val'
      case eitherMessage of
        Right msg -> pure msg
        Left txt' -> throw $ userError txt'

    eitherDecodeMessageOrThrow :: Value -> Either [Char] Message
    eitherDecodeMessageOrThrow val' = do
      let resultJSONParser = fromJSON val'
      case resultJSONParser of
        Error _ -> Left "Impossible to parse the message"
        Success msg' -> Right msg'

putContactMessage :: [Message] -> HandlerM ()
putContactMessage messages' = do
  sharedEnv <- ask
  let stmDatabase = db sharedEnv
  database' <- liftIO $ atomically $ readTMVar stmDatabase
  let newDatabase' = database' &_Object . at "ContactMessages" ?~ (Array $ V.fromList (toJSON <$> messages'))
  _ <- liftIO $ atomically $ swapTMVar stmDatabase newDatabase'
  pure ()

insertContactMessage ::Message -> HandlerM ()
insertContactMessage msg' = do
  msgs' <- getContactMessages
  _ <- putContactMessage (msgs' ++ [msg'])
  pure ()

deleteContactMessage :: UUID -> HandlerM ()
deleteContactMessage msgId' = do
  msgs' <- getContactMessages
  let newMsgs' = filter (\m -> (messageId m) /= msgId') msgs'
  _ <- putContactMessage newMsgs'
  pure ()

getStaticPage :: [(Text, Text)] -> Text -> HandlerM H.Html
getStaticPage messages' templateName = do
  $(logInfo) ("requesting: " <> (show templateName) :: Text)
  -- frontEndVariables
  let frontEndVariables = Object $ HM.fromList [
                                     ]
  let frontContext = HM.fromList [
          ("frontEndVariables", M.String . decodeUtf8 . BSL.toStrict . encode $ frontEndVariables)
        , ("messages", M.Array . V.fromList $ M.toMustache . (uncurry NotificationMustache) <$> messages')
        ] :: HM.HashMap Text M.Value
  template' <- compileTemplate (unpack templateName)
  preEscapedToMarkupSubstituteTemplate template' frontContext
