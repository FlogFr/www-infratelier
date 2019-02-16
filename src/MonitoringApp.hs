module MonitoringApp where

import Data.Function
import Control.Monad
import Control.Monad.Reader (ask)
import System.IO
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import HandlerM
import Servant
import SharedEnv
import Monitoring
import Data.Settings


type MonitoringAPI = "metrics" :> Get '[MetricContentType] [Metric]


startMonitoring :: SharedEnv -> IO ()
startMonitoring sharedEnv =
  run (monitoring_port . settings $ sharedEnv) (logStdoutDev (monitoringApp sharedEnv) )

monitoringApp :: SharedEnv -> Application
monitoringApp c = serve monitoringApi $ hoistServer monitoringApi (nt c) monitoringServer

monitoringApi :: Proxy MonitoringAPI
monitoringApi = Proxy

monitoringServer :: ServerT MonitoringAPI HandlerM
monitoringServer = getMetrics
 where
  getMetrics :: HandlerM [Metric]
  getMetrics = do
    sharedEnv <- ask
    metrics <- liftIO $ atomically $ readTMVar $ (monitoringMetrics sharedEnv)
    return [(localchef_nb_requests metrics)
           ,(localchef_thread_email_pulse metrics)]
