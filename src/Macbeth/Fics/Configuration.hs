{-# LANGUAGE DeriveGeneric #-}

module Macbeth.Fics.Configuration (
  Config(..),
  Logging(..),
  User(..),
  initConfig,
  loadConfig,
  saveConfig,
  saveCredentials
) where

import Macbeth.Utils.Utils
import Paths

import Control.Monad
import Control.Monad.Except
import System.Directory
import System.FilePath
import GHC.Generics
import qualified Data.Yaml as Y


data Config = Config {
    directory :: FilePath
  , autologin :: Bool
  , logging :: Logging
  , user :: Maybe User
} deriving (Show, Generic)


data Logging = Logging {
    stdOut :: Bool
  , file :: Bool
} deriving (Show, Generic)


data User = User {
    username :: String
  , password :: String
} deriving (Show, Generic)


instance Y.FromJSON Config
instance Y.ToJSON Config


instance Y.FromJSON Logging
instance Y.ToJSON Logging


instance Y.FromJSON User
instance Y.ToJSON User


initConfig :: IO ()
initConfig = do
  appDir <- getAppDir ""
  createDirectoryIfMissing False appDir
  configExists <- doesFileExist =<< getAppDir "macbeth.yaml"
  unless configExists $ do
    dir <- (</> "Macbeth") <$> getUserDocumentsDirectory
    createDirectoryIfMissing False dir
    saveConfig $ defaultConfig dir


loadConfig :: IO Config
loadConfig = either (error . show) return =<< runExceptT fromDisk


fromDisk :: ExceptT Y.ParseException IO Config
fromDisk = ExceptT $ getAppDir "macbeth.yaml" >>= Y.decodeFileEither


saveConfig :: Config -> IO ()
saveConfig config = getAppDir "macbeth.yaml" >>= flip Y.encodeFile config


saveCredentials :: String -> String -> IO ()
saveCredentials username password = do
  config <- loadConfig
  saveConfig $ config {user = Just $ User username (encrypt password), autologin = True}


defaultConfig :: String -> Config
defaultConfig dir = Config {
  directory = dir,
  logging = Logging False False,
  autologin = False,
  user = Nothing
}

