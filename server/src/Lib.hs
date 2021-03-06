{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module Lib
    ( app
    , api
    , Tracking (..)
    , Report (..)
    ) where

import           Data.Aeson      (ToJSON)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Text       (Text)
import           Data.Time.Clock (NominalDiffTime)
import           GHC.Generics
import           Servant

import           Sample          (ReOutput, TrOutput, reportExample,
                                  trackingExample)

-- types

newtype Tracking = Tracking { unTracking :: [[TrOutput]] }
  deriving (Generic, ToJSON)

-- I decided to bridge a list of pairs instead of an actual map,
-- because I'm missing the purescript generic instance for Map and
-- don't know how to go on from there.
newtype Report a b = Report { unReport :: [(a, b)] }
  deriving (Generic, ToJSON)

-- This constructor covers up part of the damage.
-- However, now I have to recover the Map in PureScript manually
-- or, like I did, just work with the Array instead
mkReport :: Map a b -> Report a b
mkReport = Report . Map.toList

-- api

type Api = "trackingType" :> Get '[JSON] Tracking
      :<|> "reportType"   :> Get '[JSON] (Report Text NominalDiffTime)
      :<|> Raw

api :: Proxy Api
api = Proxy

app :: Application
app = serve api handlers

-- handlers

handlers :: Server Api
handlers = pure (Tracking trackingExample)
      :<|> pure (mkReport reportExample)
      :<|> serveDirectory "site"
