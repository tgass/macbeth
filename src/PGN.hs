module PGN (
  toPGN
) where

import Api
import Move

import Data.Maybe
import Data.List

toPGN :: [Move] -> String
toPGN [] = ""
toPGN moves@(m:mx) = tagsSection m ++ "\n\n" ++ moveSection moves

moveSection :: [Move] -> String
moveSection = unwords . fmap (toString . toSAN) . filter realMove

tagsSection :: Move -> String
tagsSection m =
  "[event \"?\"]\n\
  \[site \"?\"]\n\
  \[date \"?\"]\n\
  \[Round \"?\"]\n\
  \[White \"" ++ nameW m ++ "\"]\n\
  \[Black \"" ++ nameB m ++ "\"]\n\
  \[Result \"?\"]\n\
  \[BlackElo \"?\"]\n\
  \[WhiteElo \"?\"]\n\
  \[ECO \"?\"]\n\
  \[TimeControl \"?\"]"

realMove :: Move -> Bool
realMove m = isJust $ movePretty m

toSAN :: Move -> (Int, PColor, String)
toSAN m = (moveNumber m, turn m, fromJust $ movePretty m)

toString :: (Int, PColor, String) -> String
toString (num, Black, move) = show num ++ "." ++ move
toString (_, White, move) = move
