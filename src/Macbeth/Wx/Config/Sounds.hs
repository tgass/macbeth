{-# LANGUAGE DeriveGeneric #-}

module Macbeth.Wx.Config.Sounds (
  Sounds(..),
  GameS(..),
  MoveS(..),
  EndOfGameS(..),
  RequestS(..),
  OtherS(..)
) where

import Data.Yaml
import GHC.Generics

data Sounds = Sounds {
    enabled :: Bool
  , enabledObservedGames :: Bool
  , game :: GameS
  , request :: RequestS
  , other :: OtherS
} deriving (Show, Generic)

instance FromJSON Sounds
instance ToJSON Sounds


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


data OtherS = OtherS {
  logonToServer :: Maybe String
} deriving (Show, Generic)

instance FromJSON OtherS
instance ToJSON OtherS
