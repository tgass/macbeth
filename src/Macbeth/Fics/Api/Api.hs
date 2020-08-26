module Macbeth.Fics.Api.Api where

import Data.Char
import Data.List

data Column = A | B | C | D | E | F | G | H deriving (Show, Enum, Bounded, Eq)

data Row = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Show, Eq, Enum, Bounded)

data Square = Square Column Row deriving (Eq)

instance Show Square where
  show (Square s y) = fmap toLower (show s) ++ show (fromEnum y + 1)

data PType = Pawn | Bishop | Knight | Rook | Queen | King deriving (Ord, Eq)

instance Show PType where
  show Pawn = "P"
  show Rook = "R"
  show Knight = "N"
  show Bishop = "B"
  show Queen = "Q"
  show King = "K"

data PColor = Black | White deriving (Show, Eq, Read, Ord)

data Piece = Piece PType PColor deriving (Show, Eq, Ord)

type Position = [(Square, Piece)]

data MoveDetailed = Simple Square Square | Drop Square | CastleLong | CastleShort deriving (Show, Eq)

newtype GameId = GameId Int deriving (Eq)

instance Show GameId where
  show (GameId i) = show i

instance Ord GameId where
  compare (GameId gi1) (GameId gi2) = gi1 `compare` gi2


pColor :: Piece -> PColor
pColor (Piece _ color) = color


hasColor :: PColor -> Piece -> Bool
hasColor color (Piece _ pc) = pc == color


removePiece :: Position -> Square -> Position
removePiece pos sq = filter (\(sq', _) -> sq /= sq') pos


getPiece :: Position -> Square -> Maybe Piece
getPiece p sq = sq `lookup` p


getSquare :: Position -> Piece -> Maybe Square
getSquare pos p = fst <$> find ((== p) . snd) pos


capturedPiecesWithColor :: PColor -> Position -> [Piece]
capturedPiecesWithColor color' pos =
  fmap (`Piece` color') allPieces' \\ filter (hasColor color') (fmap snd pos)
  where
    allPieces' :: [PType]
    allPieces' = replicate 8 Pawn ++ replicate 2 Rook ++ replicate 2 Knight ++
                 replicate 2 Bishop ++ [Queen, King]


invert :: PColor -> PColor
invert White = Black
invert Black = White
