module Macbeth.Utils.FEN (
  available,
  convert
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move

import Data.List.Utils


convert :: Move -> String
convert m = piecePlacement (positionRaw m) ++ " " ++
        activeColor (turn m) ++ " " ++
        castlingAv' (castlingAv m) ++ " " ++
        enPassant (turn m) (doublePawnPush m) ++ " " ++
        show (ply m) ++ " " ++
        show (moveNumber m)

available :: Move -> Bool
available m
  | moveNumber m == 1 && turn m == Black = False
  | otherwise = True

enPassant :: PColor -> Maybe Column -> String
enPassant Black (Just c) = show $ Square c Three
enPassant White (Just c) = show $ Square c Six
enPassant _ _ = "-"


castlingAv' :: [Castling] -> String
castlingAv' [] = "-"
castlingAv' cx = concatMap castlingToStr cx


castlingToStr :: Castling -> String
castlingToStr WhiteLong = "Q"
castlingToStr WhiteShort = "K"
castlingToStr BlackLong = "q"
castlingToStr BlackShort = "k"


activeColor :: PColor -> String
activeColor White = "w"
activeColor Black = "b"


piecePlacement :: String -> String
piecePlacement = replace " " "/" . emptySquares 8


emptySquares :: Int -> String -> String
emptySquares num str
  | num == 0 = str
  | otherwise = emptySquares (num-1)  (replace (replicate num '-') (show num) str)
