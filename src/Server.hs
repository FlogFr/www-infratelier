module Server
    ( startApp
    , HandlerM
    ) where

import           Protolude

import qualified Data.Aeson as A
import           Network.Wai
import           Servant.Multipart
import           Network.Wai.Handler.Warp       ( run )
import qualified Text.Mustache.Types as M
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Vector as V
import qualified Data.HashMap.Strict           as HM
                                         hiding ( map )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )
import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html5              as H
                                                ( Html )
import           HandlerM
import           SharedEnv
import           Data hiding (template)
import           Text.Mustache.Types
import Template
import Mustache
import Form
import Utils


type API = "static" :> Raw
      :<|> "blog" :> Get '[HTML] H.Html
      :<|> "newsletter" :> Get '[HTML] H.Html
      :<|> "newsletter" :> Header "X-Real-IP" Text :> MultipartForm Mem NewsletterForm :> Post '[HTML] H.Html
      :<|> "contact" :> Get '[HTML] H.Html
      :<|> "contact" :> Header "X-Real-IP" Text :> MultipartForm Mem ContactForm :> Post '[HTML] H.Html
      :<|> "404" :> Get '[HTML] H.Html
      :<|> "admin" :> Get '[HTML] H.Html
      :<|> "message" :> "delete" :> Capture "messageid" UUID :> Get '[HTML] H.Html
      :<|> Get '[HTML] H.Html

server :: ServerT API HandlerM
server =
  staticServer
  :<|> getStaticPage [] "blog.html"
  :<|> getStaticPage [] "newsletter.html"
  :<|> postNewsletter
  :<|> getStaticPage [] "contact.html"
  :<|> postContact
  :<|> getStaticPage [] "404.html"
  :<|> getAdminDashboard
  :<|> getMessageDelete
  :<|> getStaticPage [] "home.html"
  where
    staticServer = serveDirectoryFileServer "front/www"

    postNewsletter :: Maybe Text -> NewsletterForm -> HandlerM H.Html
    postNewsletter mHeaderIP form' = do
      let headerIP = fromMaybe "127.0.0.1" mHeaderIP

      if newsletterFormSecretValue form' /= "1efNoYmK60eE6icybVhkrAzDisZXLRaS"
        then throwError $ err302 {
            errHeaders = [
                ("Location", "/newsletter")
            ]
          }
        else return ()

      sub' <- liftIO $ newNewsletterSubscriber (newsletterFormEmail form') headerIP
      insertEmailToNewsletterList $ sub'

      -- frontEndVariables
      let frontEndVariables = A.Object $ HM.fromList [
                                        ]
      let frontContext = HM.fromList [
              ("frontEndVariables", String . decodeUtf8 . BSL.toStrict . A.encode $ frontEndVariables)
            , ("messages", Array . V.fromList $ toMustache . (uncurry NotificationMustache) <$> [("Thank you for your registration", "normal")])
            ] :: HM.HashMap Text Value
      template' <- compileTemplate "newsletter.html"
      preEscapedToMarkupSubstituteTemplate template' frontContext

    postContact :: Maybe Text -> ContactForm -> HandlerM H.Html
    postContact mHeaderIP form' = do
      let headerIP = fromMaybe "127.0.0.1" mHeaderIP

      if contactFormSecretValue form' /= "1efNoYmK60eE6icybVhkrAzDisZXLRaS"
        then throwError $ err302 {
            errHeaders = [
                ("Location", "/newsletter")
            ]
          }
        else return ()

      if contactFormEmail form' == ""
        then getStaticPage [("Please provide your email address", "primary")] "contact.html"
        else do
          -- insert the contact message to the database
          msg' <- liftIO $ newMessage (contactFormMessage form') (contactFormEmail form') headerIP
          _ <- insertContactMessage msg'

          -- frontEndVariables
          let frontEndVariables = A.Object $ HM.fromList [
                                            ]
          let frontContext = HM.fromList [
                  ("frontEndVariables", String . decodeUtf8 . BSL.toStrict . A.encode $ frontEndVariables)
                , ("messages", Array . V.fromList $ toMustache . (uncurry NotificationMustache) <$> [("Thank you for your message, we'll try to answer to you shortly", "normal")])
                ] :: HM.HashMap Text Value

          template' <- compileTemplate "contact.html"
          preEscapedToMarkupSubstituteTemplate template' frontContext

    getAdminDashboard :: HandlerM H.Html
    getAdminDashboard = do
      messages' <- getContactMessages
      subs' <- getNewsletterSubscribers

      -- frontEndVariables
      let frontEndVariables = A.Object $ HM.fromList [
                                        ]
      let frontContext = HM.fromList [
              ("frontEndVariables", M.String . decodeUtf8 . BSL.toStrict . A.encode $ frontEndVariables)
            , ("subscribers", M.Array . V.fromList $ M.toMustache . NewsletterSubscriberMustache <$> subs')
            , ("messages", M.Array . V.fromList $ M.toMustache . MessageMustache <$> messages')
            ] :: HM.HashMap Text M.Value
      template' <- compileTemplate "admin.html"
      preEscapedToMarkupSubstituteTemplate template' frontContext

    getMessageDelete :: UUID -> HandlerM H.Html
    getMessageDelete msgId' = do
      _ <- deleteContactMessage msgId'
      throwError $ err302 {
          errHeaders = [
              ("Location", "/admin")
          ]
        }

startApp :: SharedEnv -> IO ()
startApp sharedEnv =
  run (app_port . settings $ sharedEnv) (logStdoutDev (app sharedEnv) )

app :: SharedEnv -> Application
-- serveWithContext :: (HasServer api context) => Proxy api -> Context context -> Server api -> Application
-- hoistServer :: HasServer api '[] => Proxy api -> (forall x. m x -> n x) -> ServerT api m -> ServerT api n 
-- hoistServerWithContext :: HasServer api context => Proxy api -> Proxy context -> (forall x . m x -> n x) -> ServerT api m -> ServerT api n
app c = serve api (hoistServer api (nt c) server)

api :: Proxy API
api = Proxy
