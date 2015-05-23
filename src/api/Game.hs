module Game (
  Game (..),
  GameSettings (..),
  GameResult (..),
  turnToGameResult,
  GameInfo (..)
) where

import Api

data Game = Game { id :: Int
                 , isExample :: Bool
                 , isSetup :: Bool
                 , ratingW :: Rating
                 , namePlayerW :: String
                 , ratingPlayer2 :: Rating
                 , namePlayerB :: String
                 , settings :: GameSettings
                 } deriving (Show)

data GameSettings = GameSettings { isPrivate :: Bool
                                 , gameType :: GameType
                                 , isRated :: Bool} deriving (Show)

data GameInfo = GameInfo {
  _nameW :: String,
  _ratingW :: Rating,
  _nameB :: String,
  _ratingB :: Rating
} deriving (Show)

data GameResult = WhiteWins | BlackWins | Draw

instance Show GameResult where
  show WhiteWins = "1-0"
  show BlackWins = "0-1"
  show Draw      = "1/2-1/2"


turnToGameResult :: Color -> GameResult
turnToGameResult Black = WhiteWins
turnToGameResult White = BlackWins
