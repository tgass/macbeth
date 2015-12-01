module Macbeth.Wx.PieceSet (
  PieceSet(..),
  alpha_ead01,
  pieceSetFindSize
) where

import Data.Maybe
import Safe

data PieceSet = PieceSet { path :: FilePath, sizes :: [Int] }

alpha_ead01 = PieceSet "alpha.ead-01" [20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,52,56,60,64,72,80,88,96,112,128,144,300]

pieceSetFindSize :: PieceSet -> Int -> Int
pieceSetFindSize ps panelWidth =
  fromMaybe 300 $ headMay $ dropWhile (< round (fromIntegral panelWidth / 8)) (sizes ps)
