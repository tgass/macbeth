{-# LANGUAGE DeriveGeneric #-}

module Macbeth.Wx.Game.PieceSet (
  PieceSet(..),
  path,
  display,
  findSize
) where

import           Data.Aeson.Types
import           Data.Maybe
import           GHC.Generics
import           Safe



{-
http://ixian.com/chess/jin-piece-sets/
This work by Eric De Mund is licensed under a Creative Commons Attribution-Share Alike 3.0 Unported License
-}

data PieceSet = Alpha1 | Alpha2 | Merida1 | Merida2 | Uscf1 | Uscf2 deriving (Enum, Generic)

path :: PieceSet -> String
path Alpha1 = "alpha.ead-01"
path Alpha2 = "alpha.ead-02"
path Merida1 = "merida.ead-01"
path Merida2 = "merida.ead-02"
path Uscf1 = "uscf.ead-01"
path Uscf2 = "uscf.ead-02"


display :: PieceSet -> String
display Alpha1 = "Alpha (ead-01)"
display Alpha2 = "Alpha (ead-02)"
display Merida1 = "Merida (ead-01)"
display Merida2 = "Merida (ead-02)"
display Uscf1 = "USCF (ead-01)"
display Uscf2 = "USCF (ead-02)"


sizes :: [Int]
sizes = [20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,52,56,60,64,72,80,88,96,112,128,144,300]

-- returns piece size and scale
findSize :: Int -> (Int, Double)
findSize panelWidth =
  let psize = fromMaybe 300 $ headMay $ dropWhile (< round (fromIntegral panelWidth / (8 :: Double))) sizes
  in (psize, fromIntegral panelWidth / (8 :: Double) / fromIntegral psize)

instance ToJSON PieceSet
instance FromJSON PieceSet

