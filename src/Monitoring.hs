{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monitoring
    ( MetricContentType
    , MonitoringMetrics(..)
    , Metric(..)
    , initMonitoringMetrics
    ) where

import GHC.Generics
import Network.HTTP.Media ((//))
import Servant
import Data.Int
import Data.Function
import Data.Monoid
import Text.Show
import Data.Aeson
import Data.Eq
import Data.Text as T
import Data.Text.Encoding as E
import Data.ByteString.Lazy as L

data MetricContentType

data Metric = Counter { name :: T.Text
              , help :: T.Text
              , value :: Int
              } | Gauge { name :: T.Text
              , help :: T.Text
              , value :: Int
              } deriving (Show, Generic, Eq)

instance ToJSON Metric
instance FromJSON Metric

instance Accept MetricContentType where
  contentType _ = "text" // "plain"

instance MimeRender MetricContentType Metric where
  -- Example of mashalling:
  --
  -- # HELP node_vmstat_thp_file_alloc /proc/vmstat information field thp_file_alloc.
  -- # TYPE node_vmstat_thp_file_alloc untyped
  -- node_vmstat_thp_file_alloc 0
  mimeRender _ (Counter cname chelp cvalue) = L.fromStrict . E.encodeUtf8 $
    ("# HELP " <> cname <> " " <> chelp <> ".\n" <>
     "# TYPE " <> cname <> " counter.\n" <>
     cname <> " " <> (T.pack . show $ cvalue))
  mimeRender _ (Gauge cname chelp cvalue) = L.fromStrict . E.encodeUtf8 $
    ("# HELP " <> cname <> " " <> chelp <> ".\n" <>
     "# TYPE " <> cname <> " gauge.\n" <>
     cname <> " " <> (T.pack . show $ cvalue))

instance MimeRender MetricContentType ([Metric]) where
   mimeRender _ [] = ""
   mimeRender t (metric:ms) = (mimeRender t metric) <> "\n" <> (mimeRender t ms)

data MonitoringMetrics = MonitoringMetrics
  { localchef_nb_requests :: Metric
  , localchef_thread_email_pulse :: Metric } 

initMonitoringMetrics :: MonitoringMetrics
initMonitoringMetrics =
  MonitoringMetrics
    (Counter "localchef_nb_requests" "nb of requests per backend" 0)
    (Counter "localchef_thread_email_pulse" "pulse of the thread email" 0)
