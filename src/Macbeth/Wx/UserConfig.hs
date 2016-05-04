{-# LANGUAGE DeriveGeneric #-}

module Macbeth.Wx.UserConfig (
  Config(..),
  User(..),
  Sounds(..),
  GameS(..),
  MoveS(..),
  EndOfGameS(..),
  RequestS(..),
  CountdownS(..),
  OtherS(..),
  initConfig,
  loadConfig,
  saveConfig,
  saveCredentials
) where

import Macbeth.Utils.Utils
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


data Sounds = Sounds {
    enable :: Bool
  , enableObservedGames :: Bool
  , chat :: ChatS
  , game :: GameS
  , request :: RequestS
  , countdown :: CountdownS
  , notifications :: NotificationsS
  , other :: OtherS
} deriving (Show, Generic)

instance FromJSON Sounds
instance ToJSON Sounds


data ChatS = ChatS {
    say :: Maybe Bool
  , privateTell :: Maybe Bool
  , kibitz :: Maybe Bool
  , whisper :: Maybe Bool
  , shout :: Maybe Bool
  , cShout :: Maybe Bool
  , partnerTell :: Maybe Bool
} deriving (Show, Generic)

instance FromJSON ChatS
instance ToJSON ChatS


data GameS = GameS {
    newGame :: Maybe String
  , move :: MoveS
  , endOfGame :: EndOfGameS
} deriving (Show, Generic)

instance FromJSON GameS
instance ToJSON GameS


data MoveS = MoveS {
    normal :: Maybe String
  , capture :: Maybe String
  , check :: Maybe String
  , castling :: Maybe String
  , pieceDrop :: Maybe String
  , illegal :: Maybe String
--  , explosion :: Maybe String
--  , cannotSmartmove :: Maybe String
} deriving (Show, Generic)

instance FromJSON MoveS
instance ToJSON MoveS

data EndOfGameS = EndOfGameS {
    checkmate :: Maybe String
  , youWin :: Maybe String
  , youLose :: Maybe String
  , draw :: Maybe String
  , abort :: Maybe String
} deriving (Show, Generic)

instance FromJSON EndOfGameS
instance ToJSON EndOfGameS

data RequestS = RequestS {
    challenge :: Maybe String
  , abortGame :: Maybe String
  , drawOffer :: Maybe String
  , adjourn :: Maybe String
  , takeback :: Maybe String
  , pause :: Maybe String
  , unpause :: Maybe String
} deriving (Show, Generic)

instance FromJSON RequestS
instance ToJSON RequestS


data CountdownS = CountdownS {
    _10_sec :: Maybe String
  , _30_sec :: Maybe String
  , _60_sec :: Maybe String
  , _120_sec :: Maybe String
} deriving (Show, Generic)

instance FromJSON CountdownS
instance ToJSON CountdownS

data NotificationsS = NotificationsS {

} deriving (Show, Generic)

instance FromJSON NotificationsS
instance ToJSON NotificationsS


data OtherS = OtherS {
  logonToServer :: Maybe String
} deriving (Show, Generic)

instance FromJSON OtherS
instance ToJSON OtherS

initConfig :: IO Config
initConfig = do
  appDir <- getAppDir ""
  createDirectoryIfMissing False appDir
  configExists <- doesFileExist =<< getAppDir "macbeth.yaml"
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
  user = Nothing,
  sounds = defaultSounds
}

defaultSounds :: Maybe Sounds
defaultSounds = Just Sounds {
    enable = True
  , enableObservedGames = True
  , chat = defChatSnds
  , game = defGameSnds
  , request = defRequestSnds
  , countdown = defCountdownS
  , notifications = NotificationsS
  , other = defOtherS
}


defChatSnds :: ChatS
defChatSnds = ChatS {
    say = Nothing
  , privateTell = Nothing
  , kibitz = Nothing
  , whisper = Nothing
  , shout = Nothing
  , cShout = Nothing
  , partnerTell = Nothing
}

defGameSnds :: GameS
defGameSnds = GameS {
    newGame = Just "ding1.wav"
  , move = MoveS {
      normal = Just "woodthunk.wav"
    , capture = Nothing
    , check = Just "squeak.wav"
    , castling = Just "ching.wav"
    , pieceDrop = Nothing
    , illegal = Just "penalty.wav"
--    , explosion = Nothing
--    , cannotSmartmove = Nothing
  }
  , endOfGame = EndOfGameS {
      youWin = Nothing
    , youLose = Nothing
    , checkmate = Nothing
    , draw = Nothing
    , abort = Nothing
  }
}

defRequestSnds :: RequestS
defRequestSnds = RequestS {
    challenge = Nothing
  , abortGame = Nothing
  , drawOffer = Nothing
  , adjourn = Nothing
  , takeback = Nothing
  , pause = Nothing
  , unpause = Nothing
}

defCountdownS :: CountdownS
defCountdownS = CountdownS {
    _10_sec = Nothing
  , _30_sec = Nothing
  , _60_sec = Nothing
  , _120_sec = Nothing
}

defOtherS :: OtherS
defOtherS = OtherS {
  logonToServer = Nothing
}
