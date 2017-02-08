module Macbeth.Fics.Api.Game (
  GameId(..),
  GameProperties(..),
  Game (..),
  GameType (..),
  GameSettings (..),
  GameInfo (..),
  userColor,
  toTitle
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Player
import Macbeth.Fics.Api.Rating

newtype GameId = GameId Int deriving (Eq)

instance Show GameId where
  show (GameId i) = show i

instance Ord GameId where
  compare (GameId gi1) (GameId gi2) = gi1 `compare` gi2

data GameProperties = GameProperties {
    gameId' :: GameId
  , playerW' :: Username
  , playerB' :: Username
  , isGameUser' :: Bool } deriving (Show, Eq)

data GameType =
  Lightning | Blitz | Standard | Wild | Atomic | Crazyhouse | Bughouse | Losers |
  Suicide | Untimed | ExaminedGame | NonStandardGame  deriving (Show, Eq, Ord)

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


toTitle :: GameProperties -> String
toTitle (GameProperties id' pw pb _) =  "[Game " ++ show id' ++ "] " ++ pw ++ " vs. " ++ pb


userColor :: GameProperties -> Username -> Maybe PColor
userColor (GameProperties _ _ _ False) _ = Nothing
userColor (GameProperties _ playerW _ True) username
  | playerW == username = Just White
  | otherwise = Just Black
