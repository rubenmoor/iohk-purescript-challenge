{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Monad.Reader.Class                (MonadReader)
import           Data.Proxy
import           Language.PureScript.Bridge                (BridgePart,
                                                            FullBridge, SumType,
                                                            buildBridge,
                                                            defaultBridge,
                                                            mkSumType, typeName,
                                                            writePSTypes, (<|>),
                                                            (^==))
import           Language.PureScript.Bridge.Builder        (BridgeData,
                                                            psTypeParameters)
import           Language.PureScript.Bridge.TypeInfo       (Language (Haskell),
                                                            PSType,
                                                            TypeInfo (..))
import           Language.PureScript.Bridge.TypeParameters (A, B)

import           Lib                                       (Report, Tracking,
                                                            api)

types :: [SumType Haskell]
types =
  [ mkSumType (Proxy :: Proxy (Report A B))
  , mkSumType (Proxy :: Proxy Tracking)
  ]

-- custom bridges

psSeconds :: PSType
psSeconds = TypeInfo "purescript-datetime"
                     "Data.Time.Duration"
                     "Seconds"
                     []

nominalDiffTimeBridge :: BridgePart
nominalDiffTimeBridge =
  typeName ^== "NominalDiffTime" >> pure psSeconds

bridge :: FullBridge
bridge = buildBridge $ defaultBridge <|> nominalDiffTimeBridge

-- ps generator

main :: IO ()
main = writePSTypes "../client/src" bridge types
