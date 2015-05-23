{-# LANGUAGE OverloadedStrings #-}

module Api (
  PColor (..),
  Piece (..),
  PType (..),
  Square (..),
  Row (..),
  Column (..),
  Position,
  pColor,
  invert,
  removePiece,
  getPiece
) where

import Data.Char (toLower)

data Column = A | B | C | D | E | F | G | H deriving (Show, Enum, Ord, Eq)

data Row = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Ord, Show, Enum, Bounded)

data PType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq)

data PColor = Black | White deriving (Eq, Show)

data Piece = Piece PType PColor deriving (Show, Eq)

data Square = Square Column Row deriving (Ord, Eq)

instance Show Square where
  show (Square s y) = fmap toLower (show s) ++ show (fromEnum y + 1)

type Position = [(Square, Piece)]


pColor :: Piece -> PColor
pColor (Piece _ color) = color


invert :: PColor -> PColor
invert White = Black
invert Black = White


removePiece :: Position -> Square -> Position
removePiece pos sq = filter (\(sq', _) -> sq /= sq') pos


getPiece :: Position -> Square -> PColor -> Maybe Piece
getPiece pos sq color = sq `lookup` pos >>= checkColor color
  where
    checkColor :: PColor -> Piece -> Maybe Piece
    checkColor c p@(Piece _ c') = if c == c' then Just p else Nothing

