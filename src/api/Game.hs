module Game (
  Game (..),
  GameSettings (..)
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

