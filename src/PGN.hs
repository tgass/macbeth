module PGN (
  moveSection
) where

import Api
import Move

import Data.Maybe
import Data.List

moveSection :: [Move] -> String
moveSection = concat . (intersperse " ") . fmap (toString . toSAN) . filter realMove

realMove :: Move -> Bool
realMove m = isJust $ movePretty m

toSAN :: Move -> (Int, Color, String)
toSAN m = (moveNumber m, turn m, fromJust $ movePretty m)

toString :: (Int, Color, String) -> String
toString (num, Black, move) = show num ++ "." ++ move
toString (_, White, move) = move
