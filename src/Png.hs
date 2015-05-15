module Png (

) where

import Api
import Move

import Date.Time.Clock
import Date.Time.Calendar


--getCurrentTime >>= return . utctDay

header :: Move -> GameResult -> String
header move result =
  "[date \"2015.05.13\"]\
  \[event \"FICS online\"] \
  \[site \"?\"]\
  \[Round \"?\"]\
  \[White \"" ++ nameW move ++"\"]\
  \[Black \"" ++ nameB move ++ "\"]\
  \[Result \"" ++ show result ++ "\"]\
  \[WhiteElo \"?\"]\
  \[BlackElo \"?\"]\
  \[ECO \"?\"]\
  \[TimeControl \"900+15\"]"
