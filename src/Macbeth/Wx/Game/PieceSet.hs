module Macbeth.Wx.Game.PieceSet (
  PieceSet(..),
  pieceSets,
  pieceSetFindSize
) where

import Data.Maybe
import Safe


{-
http://ixian.com/chess/jin-piece-sets/
This work by Eric De Mund is licensed under a Creative Commons Attribution-Share Alike 3.0 Unported License
-}

data PieceSet = PieceSet { path :: FilePath, display :: String }

pieceSets :: [PieceSet]
pieceSets = [
    PieceSet "alpha.ead-01" "Alpha (ead-01)"
  , PieceSet "alpha.ead-02" "Alpha (ead-02)"
  , PieceSet "merida.ead-01" "Merida (ead-01)"
  , PieceSet "merida.ead-02" "Merida (ead-02)"
  , PieceSet "uscf.ead-01" "USCF (ead-01)"
  , PieceSet "uscf.ead-02" "USCF (ead-02)"]

pieceSetFindSize :: Int -> Int
pieceSetFindSize panelWidth =
  fromMaybe 300 $ headMay $ dropWhile (< round (fromIntegral panelWidth / (8 :: Double))) sizes
  where sizes = [20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,52,56,60,64,72,80,88,96,112,128,144,300]
