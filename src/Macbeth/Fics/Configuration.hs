{-# LANGUAGE DeriveGeneric #-}
module Macbeth.Fics.Configuration (
  Config(..),
  Logging(..),
  loadConfig,
  loadDefaultConfig,
  saveConfig
) where

import Paths

import Control.Monad.Except
import Data.Either.Unwrap
import System.Directory
import System.FilePath
import GHC.Generics
import qualified Data.Yaml as Y

data Config = Config {
  directory :: Maybe FilePath,
  logging :: Logging
} deriving (Show, Generic)

data Logging = Logging {
  stdOut :: Bool,
  file :: Bool
} deriving (Show, Generic)

instance Y.FromJSON Config
instance Y.ToJSON Config

instance Y.FromJSON Logging
instance Y.ToJSON Logging


loadConfig :: IO Config
loadConfig = fmap fromRight $ runExceptT $ fromDisk `catchError` (\_ -> return defaultConfig)

loadDefaultConfig :: IO Config
loadDefaultConfig = do
  dir <- getDefaultDirectory Nothing
  return $ defaultConfig {directory = Just dir}


-- | If no directory is set, set default directory ~/Macbeth
fromDisk :: ExceptT Y.ParseException IO Config
fromDisk = do
  config <- ExceptT $ getDataFileName "macbeth.yaml" >>= Y.decodeFileEither
  dir <- liftIO $ getDefaultDirectory (directory config)
  return $ config {directory = Just dir}

-- | Create default directory if it doesn't exist
getDefaultDirectory :: Maybe FilePath -> IO FilePath
getDefaultDirectory Nothing = do
  dir <- (</> "Macbeth") `fmap` getUserDocumentsDirectory
  createDirectoryIfMissing False dir
  return dir
getDefaultDirectory (Just path) = return path

saveConfig :: Config -> IO ()
saveConfig config = getDataFileName "macbeth.yaml" >>= flip Y.encodeFile config

defaultConfig = Config {
  directory = Nothing,
  logging = Logging False False
}

