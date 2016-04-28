{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Macbeth.Fics.AppConfig (
  AppConfig(..),
  loadAppConfig
) where

import Paths

import Control.Monad.Except
import Data.Aeson.Types
import Data.Yaml
import GHC.Generics
import System.Log.Logger


data AppConfig = AppConfig {
  priority :: Priority
} deriving (Show, Generic)


instance ToJSON AppConfig
instance FromJSON AppConfig


loadAppConfig :: IO AppConfig
loadAppConfig = either (error . prettyPrintParseException) return =<< runExceptT fromDisk


fromDisk :: ExceptT ParseException IO AppConfig
fromDisk = ExceptT $ getDataFileName "appConfig.yaml" >>= decodeFileEither


instance ToJSON Priority where
  toJSON DEBUG = String "DEBUG"
  toJSON INFO = String "INFO"
  toJSON NOTICE = String "NOTICE"
  toJSON WARNING = String "WARNING"
  toJSON ERROR = String "ERROR"
  toJSON CRITICAL = String "CRITICAL"
  toJSON ALERT = String "ALERT"
  toJSON EMERGENCY = String "EMERGENCY"


instance FromJSON Priority where
  parseJSON (String "DEBUG") = pure DEBUG
  parseJSON (String "INFO") = pure INFO
  parseJSON (String "NOTICE") = pure NOTICE
  parseJSON (String "WARNING") = pure WARNING
  parseJSON (String "ERROR") = pure ERROR
  parseJSON (String "CRITICAL") = pure CRITICAL
  parseJSON (String "ALERT") = pure ALERT
  parseJSON (String "EMERGENCY") = pure EMERGENCY

  parseJSON invalid = typeMismatch "priority" invalid

