module Macbeth.Api.Game (
  Game (..),
  GameType (..),
  GameSettings (..),
  GameResult (..),
  GameInfo (..)
) where

import Macbeth.Api.Rating

data GameType =  Blitz | Lightning | Untimed | ExaminedGame | Standard | Wild | Atomic |
                 Crazyhouse | Bughouse | Losers | Suicide | NonStandardGame  deriving (Show, Eq)

data Game = Game {
    id :: Int
  , isExample :: Bool
  , isSetup :: Bool
  , ratingW :: Rating
  , nameW :: String
  , ratingB :: Rating
  , nameB :: String
  , settings :: GameSettings } deriving (Show, Eq)

data GameSettings = GameSettings {
    isPrivate :: Bool
  , gameType :: GameType
  , isRated :: Bool} deriving (Show, Eq)

data GameInfo = GameInfo {
    _nameW :: String
  , _ratingW :: Rating
  , _nameB :: String
  , _ratingB :: Rating
} deriving (Show, Eq)

data GameResult = WhiteWins | BlackWins | Draw | Aborted deriving (Eq)

instance Show GameResult where
  show WhiteWins = "1-0"
  show BlackWins = "0-1"
  show Draw      = "1/2-1/2"
  show Aborted   = ""

