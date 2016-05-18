module Macbeth.Fics.Api.Game (
  GameId(..),
  Game (..),
  GameType (..),
  GameSettings (..),
  GameInfo (..)
) where

import Macbeth.Fics.Api.Rating

newtype GameId = GameId Int deriving (Eq)

instance Show GameId where
  show (GameId i) = show i

instance Ord GameId where
  compare (GameId gi1) (GameId gi2) = gi1 `compare` gi2

data GameType =  Blitz | Lightning | Untimed | ExaminedGame | Standard | Wild | Atomic |
                 Crazyhouse | Bughouse | Losers | Suicide | NonStandardGame  deriving (Show, Eq)

data Game = Game {
    gameId :: GameId
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
