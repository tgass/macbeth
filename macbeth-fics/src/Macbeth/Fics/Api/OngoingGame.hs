module Macbeth.Fics.Api.OngoingGame where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Rating


data OngoingGame = OngoingGame {
    gameId :: GameId
  , isExample :: Bool
  , isSetup :: Bool
  , ratingW :: Rating
  , nameW :: String
  , ratingB :: Rating
  , nameB :: String
  , settings :: GameSettings
} deriving (Show, Eq)


data GameSettings = GameSettings {
    isPrivate :: Bool
  , gameType :: GameType
  , isRated :: Bool
} deriving (Show, Eq)


data GameType = Lightning | Blitz | Standard | Wild | Atomic | Crazyhouse |
  Bughouse | Losers | Suicide | Untimed | ExaminedGame | NonStandardGame deriving (Show, Eq, Ord)
