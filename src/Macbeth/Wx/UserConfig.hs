{-# LANGUAGE DeriveGeneric #-}

module Macbeth.Wx.UserConfig (
  Config(..),
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
import Data.Yaml
import GHC.Generics
import System.Directory
import System.FilePath


data Config = Config {
    directory :: FilePath
  , autologin :: Bool
  , fontSize :: Int
  , user :: Maybe User
} deriving (Show, Generic)


data User = User {
    username :: String
  , password :: String
} deriving (Show, Generic)


instance FromJSON Config
instance ToJSON Config


instance FromJSON User
instance ToJSON User


initConfig :: IO Config
initConfig = do
  appDir <- getAppDir ""
  createDirectoryIfMissing False appDir
  configExists <- doesFileExist =<< getAppDir "macbeth.yaml"
  unless configExists $ do
    dir <- (</> "Macbeth") <$> getUserDocumentsDirectory
    createDirectoryIfMissing False dir
    saveConfig $ defaultConfig dir
  loadConfig


loadConfig :: IO Config
loadConfig = either (error . prettyPrintParseException) return =<< runExceptT fromDisk


fromDisk :: ExceptT ParseException IO Config
fromDisk = ExceptT $ getAppDir "macbeth.yaml" >>= decodeFileEither


saveConfig :: Config -> IO ()
saveConfig config = getAppDir "macbeth.yaml" >>= flip encodeFile config


saveCredentials :: String -> String -> IO ()
saveCredentials username password = do
  config <- loadConfig
  saveConfig $ config {user = Just $ User username (encrypt password), autologin = True}


defaultConfig :: String -> Config
defaultConfig dir = Config {
  fontSize = 12,
  directory = dir,
  autologin = False,
  user = Nothing
}

