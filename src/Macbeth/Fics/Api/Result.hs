module Macbeth.Fics.Api.Result (
  Result(..),
  GameResult(..),
  toString
) where

import Macbeth.Fics.Api.Game

data Result = Result {
    gameId :: GameId
  , playerW :: String
  , playerB :: String
  , reason :: String
  , result :: GameResult
} deriving (Show, Eq)

toString :: Result -> String
toString r = show (result r) ++ " " ++ reason r


data GameResult = WhiteWins | BlackWins | Draw | Aborted deriving (Eq)

instance Show GameResult where
  show WhiteWins = "1-0"
  show BlackWins = "0-1"
  show Draw      = "1/2-1/2"
  show Aborted   = ""
