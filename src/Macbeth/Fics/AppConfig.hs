{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Macbeth.Fics.AppConfig (
  AppConfig(..),
  Stage(..),
  loadAppConfig
) where

import Paths

import Control.Monad.Except
import Data.Aeson.Types
import Data.Yaml
import GHC.Generics


data AppConfig = AppConfig {
  stage :: Stage
} deriving (Show, Generic)


data Stage = Prod | Dev deriving (Show, Eq, Generic)

instance ToJSON AppConfig
instance FromJSON AppConfig


loadAppConfig :: IO AppConfig
loadAppConfig = either (error . prettyPrintParseException) return =<< runExceptT fromDisk


fromDisk :: ExceptT ParseException IO AppConfig
fromDisk = ExceptT $ getDataFileName "appConfig.yaml" >>= decodeFileEither


instance ToJSON Stage where
  toJSON Dev = String "Dev"
  toJSON Prod = String "Prod"


instance FromJSON Stage where
  parseJSON (String "Dev") = pure Dev
  parseJSON (String "Prod") = pure Prod

  parseJSON invalid = typeMismatch "priority" invalid

