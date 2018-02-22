{-# LANGUAGE FlexibleContexts #-}

module Macbeth.Wx.GameType (
  Category (..),
  Board (..),
  gameTypes,
  gameTypeSelectionToString,
  onSelectGameTypeCategory
) where

import Macbeth.Wx.Utils

import Data.Char
import Data.Map
import Graphics.UI.WX

data Category = Chess | Suicide | Losers | Atomic | Wild | Crazyhouse | Bughouse deriving (Eq, Ord, Show, Read)

data Board = Board {_id :: String, display :: String} deriving (Show, Read)

gameTypes :: Map Category [Board]
gameTypes = fromList [ (Chess, []), (Suicide, []), (Losers, []), (Atomic, []),
                       (Wild, [    Board "w1" "Reversed King and Queen"
                                 , Board "w2" "Shuffle position"
                                 , Board "w3" "Shuffle position, mirrored"
                                 , Board "w4" "Random pieces, balanced bishops"
                                 , Board "w5" "Pawns on 7th rank"
                                 , Board "w8" "Pawns on 4th rank"
                                 , Board "w8a" "Pawns on 5th rank"
                                 , Board "wild fr" "Fisher Random"])
                     , (Crazyhouse, [])
                     , (Bughouse, [])]


gameTypeSelectionToString :: Category -> Maybe Board -> String
gameTypeSelectionToString _ (Just board) = _id board
gameTypeSelectionToString cat _ = fmap toLower (show cat)


onSelectGameTypeCategory :: Choice () -> Choice () -> IO ()
onSelectGameTypeCategory c_boards c_category = do
  category <- fmap read (getDisplaySelection c_category)
  set c_boards [ items := fmap display $ gameTypes ! category]
