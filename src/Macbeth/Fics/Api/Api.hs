module Macbeth.Fics.Api.Api (
  PColor (..),
  Piece (..),
  PType (..),
  Square (..),
  Row (..),
  Column (..),
  Position,
  MoveDetailed (..),
  pColor,
  hasColor,
  removePiece,
  getPiece,
  invert
) where

import Data.Char

data Column = A | B | C | D | E | F | G | H deriving (Show, Enum, Bounded, Eq)

data Row = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Show, Eq, Enum, Bounded)

data Square = Square Column Row deriving (Eq)

instance Show Square where
  show (Square s y) = fmap toLower (show s) ++ show (fromEnum y + 1)

data PType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Ord, Eq)

instance Show PType where
  show Pawn = "P"
  show Rook = "R"
  show Knight = "N"
  show Bishop = "B"
  show Queen = "Q"
  show King = "K"

data PColor = Black | White deriving (Show, Eq, Read)

data Piece = Piece PType PColor deriving (Show, Eq)

type Position = [(Square, Piece)]

data MoveDetailed = Simple Square Square | Drop Square | CastleLong | CastleShort deriving (Show, Eq)

pColor :: Piece -> PColor
pColor (Piece _ color) = color

hasColor :: PColor -> Piece -> Bool
hasColor color (Piece _ pc) = pc == color

removePiece :: Position -> Square -> Position
removePiece pos sq = filter (\(sq', _) -> sq /= sq') pos

getPiece :: Position -> Square -> Maybe Piece
getPiece p sq = sq `lookup` p

invert :: PColor -> PColor
invert White = Black
invert Black = White
