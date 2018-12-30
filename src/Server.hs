module Server
    ( startApp
    , HandlerM
    ) where

import           Protolude
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )
import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html5              as H
                                                ( Html )
import           HandlerM
import qualified Data.HashMap.Strict           as HM
                                         hiding ( map )
import           SharedEnv
import  Template
import           Data hiding (template)
import Css
import           Text.Mustache.Types


type API = "static" :> Raw
    :<|> Get '[HTML] H.Html

server :: ServerT API HandlerM
server =
  staticServer
  :<|> getHomePage
  where
    staticServer = serveDirectoryFileServer "www"

    getHomePage :: HandlerM H.Html
    getHomePage = do
      css <- haskraftCSSText
      -- frontEndVariables
      let frontContext = Object $ HM.fromList [
              ("css", String css)
            ]

      template <- compileTemplate "home.html"
      preEscapedToMarkupSubstituteTemplate template frontContext

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
