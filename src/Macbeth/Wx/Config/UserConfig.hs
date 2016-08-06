{-# LANGUAGE DeriveGeneric #-}

module Macbeth.Wx.Config.UserConfig (
  Config(..),
  User(..),
  Sounds(..),
  GameS(..),
  MoveS(..),
  EndOfGameS(..),
  RequestS(..),
  OtherS(..),
  initConfig,
  loadConfig,
  saveConfig,
  saveCredentials,
  isSoundEnabled
) where

import Macbeth.Utils.Utils
import Macbeth.Wx.Config.Sounds
import Macbeth.Wx.Config.DefaultSounds
import Paths

import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.Yaml
import GHC.Generics
import System.Directory
import System.FilePath


data Config = Config {
    directory :: FilePath
  , autologin :: Bool
  , fontSize :: Int
  , user :: Maybe User
  , sounds :: Maybe Sounds
} deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config


data User = User {
    username :: String
  , password :: String
} deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

initConfig :: IO Config
initConfig = do
  createDirectoryIfMissing False =<< getMacbethUserDataDir ""
  configExists <- doesFileExist =<< getMacbethUserDataDir "macbeth.yaml"
  unless configExists $ do
    dir <- (</> "Macbeth") <$> getUserDocumentsDirectory
    createDirectoryIfMissing False dir
    saveConfig $ defaultConfig dir
  config <- loadConfig
  when (isNothing $ sounds config) $ saveConfig config {sounds = defaultSounds}
  loadConfig


loadConfig :: IO Config
loadConfig = either (error . prettyPrintParseException) return =<< runExceptT fromDisk


fromDisk :: ExceptT ParseException IO Config
fromDisk = ExceptT $ getMacbethUserDataDir "macbeth.yaml" >>= decodeFileEither


saveConfig :: Config -> IO ()
saveConfig config = getMacbethUserDataDir    "macbeth.yaml" >>= flip encodeFile config


saveCredentials :: String -> String -> IO ()
saveCredentials username password = do
  config <- loadConfig
  saveConfig $ config {user = Just $ User username (encrypt password), autologin = True}


isSoundEnabled :: Config -> Bool
isSoundEnabled config = case sounds config of
  Just soundsConf
    | enabled soundsConf -> True
    | otherwise -> False
  _ -> False


defaultConfig :: String -> Config
defaultConfig dir = Config {
  fontSize = 12,
  directory = dir,
  autologin = False,
  user = Nothing,
  sounds = defaultSounds
}


