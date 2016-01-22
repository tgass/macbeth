{-# LANGUAGE FlexibleContexts #-}

module Macbeth.Wx.GameType (
  Category (..),
  Board (..),
  gameTypes,
  gameTypeIdxToString,
  onSelectGameTypeCategory
) where

import Data.Char
import Graphics.UI.WX

data Category = Chess | Suicide | Losers | Atomic | Wild | Crazyhouse deriving (Show)

data Board = Board {_id :: String, display :: String} deriving (Show)

gameTypes = [ (Chess, []), (Suicide, []), (Losers, []), (Atomic, []),
              (Wild, [ Board "w1" "Reversed King and Queen"
                     , Board "w2" "Shuffle position"
                     , Board "w3" "Shuffle position, mirrored"
                     , Board "w4" "Random pieces, balanced bishops"
                     , Board "w5" "Pawns on 7th rank"
                     , Board "w8" "Pawns on 4th rank"
                     , Board "w8a" "Panwns on 5th rank"
                     , Board "wild fr" "Fisher Random"])
            , (Crazyhouse, [])]

gameTypeIdxToString :: Int -> Int -> String
gameTypeIdxToString cId bId
  | bId == -1 = fmap toLower $ show $ fst $ gameTypes !! cId
  | otherwise = _id $ snd (gameTypes !! cId) !! bId

onSelectGameTypeCategory :: (Selection w, Items w String) => Choice () -> w -> IO ()
onSelectGameTypeCategory c_boards w = do
  i <- get w selection
  set c_boards [ items := fmap display $ snd $ gameTypes !! i]
