{-# LANGUAGE DeriveGeneric #-}

module Macbeth.Wx.Config.Sounds (
  Sounds(..),
  GameS(..),
  MoveS(..),
  EndOfGameS(..),
  RequestS(..),
  ChatS(..),
  OtherS(..),
  defaultSounds,
  chatS
) where

import Data.Yaml
import GHC.Generics

data Sounds = Sounds {
    enabled :: Bool
  , enabledObservedGames :: Bool
  , game :: GameS
  , request :: RequestS
  , chat :: Maybe ChatS
  , other :: OtherS
} deriving (Show, Generic)

instance ToJSON Sounds
instance FromJSON Sounds


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
  , takeback :: Maybe String
} deriving (Show, Generic)

instance FromJSON MoveS
instance ToJSON MoveS

data EndOfGameS = EndOfGameS {
    youWin :: Maybe String
  , youLose :: Maybe String
  , youDraw :: Maybe String
  , whiteWins :: Maybe String
  , blackWins :: Maybe String
  , draw :: Maybe String
  , abort :: Maybe String
} deriving (Show, Generic)

instance FromJSON EndOfGameS
instance ToJSON EndOfGameS

data RequestS = RequestS {
    challenge :: Maybe String
  , abortReq :: Maybe String
  , drawReq :: Maybe String
  , takebackReq :: Maybe String
} deriving (Show, Generic)

instance FromJSON RequestS
instance ToJSON RequestS


data ChatS = ChatS {
    say :: Maybe String
  , privateTell :: Maybe String
  , kibitz :: Maybe String
  , whisper :: Maybe String
  , shout :: Maybe String
  , cShout :: Maybe String
  , partnerTell :: Maybe String
} deriving (Show, Generic)

instance FromJSON ChatS
instance ToJSON ChatS


data OtherS = OtherS {
  logonToServer :: Maybe String
} deriving (Show, Generic)

instance FromJSON OtherS
instance ToJSON OtherS

defaultSounds :: Sounds
defaultSounds = Sounds {
    enabled = True
  , enabledObservedGames = True
  , game = gameS
  , request = requestS
  , chat = Just chatS
  , other = otherS
}

gameS :: GameS
gameS = GameS {
    newGame = Nothing
  , move = MoveS {
      normal = Just "move.wav"
    , capture = Nothing
    , check = Nothing
    , castling = Nothing
    , pieceDrop = Nothing
    , illegal = Just "penalty.wav"
    , takeback = Nothing
  }
  , endOfGame = EndOfGameS {
      youWin = Just "win.wav"
    , youLose = Just "lose.wav"
    , youDraw = Just "draw.wav"
    , whiteWins = Just "cymbal.wav"
    , blackWins = Just "cymbal.wav"
    , draw = Just "cymbal.wav"
    , abort = Nothing
  }
}

requestS :: RequestS
requestS = RequestS {
    challenge = Just "challenge.wav"
  , abortReq = Just "pop2.wav"
  , drawReq = Just "pop2.wav"
  , takebackReq = Just "pop2.wav"
}

chatS :: ChatS
chatS = ChatS {
    say = Nothing
  , privateTell = Nothing
  , kibitz = Nothing
  , whisper = Nothing
  , shout = Nothing
  , cShout = Nothing
  , partnerTell = Nothing
}

otherS :: OtherS
otherS = OtherS {
  logonToServer = Just "ding1.wav"
}
