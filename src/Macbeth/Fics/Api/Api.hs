{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Api.Api (
  PColor (..),
  Piece (..),
  PType (..),
  Square (..),
  Row (..),
  Column (..),
  Position,
  PieceMove (..),
  MoveDetailed (..),
  Username,
  diffPosition,
  movePiece,
  movePieces,
  pColor,
  invert
) where

import Data.Char
import Data.List

data Column = A | B | C | D | E | F | G | H deriving (Show, Enum, Eq)

data Row = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Show, Eq, Enum, Bounded)

data Square = Square Column Row deriving (Eq)

instance Show Square where
  show (Square s y) = fmap toLower (show s) ++ show (fromEnum y + 1)

data PType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq)

data PColor = Black | White deriving (Show, Eq)

data Piece = Piece PType PColor deriving (Show, Eq)

type Position = [(Square, Piece)]

data PieceMove = PieceMove { piece :: Piece, from :: Square, to :: Square }

instance Show PieceMove where
  show (PieceMove _ s1 s2) = show s1 ++ show s2

data MoveDetailed = Simple Square Square | CastleLong | CastleShort deriving (Show, Eq)

type Username = String

diffPosition :: Position -> Position -> [PieceMove]
diffPosition before after =
  let from = before \\ after
      to = after \\ before
  in [PieceMove piece1 s1 s2 | (s1, piece1) <- from, (s2, piece2) <- to, piece1 == piece2, s1 /= s2 ]

movePiece :: PieceMove -> Position -> Position
movePiece (PieceMove piece from to) position =
  filter (\(s, _) -> s /= from && s /= to) position ++ [(to, piece)]

movePieces :: [PieceMove] -> Position -> Position
movePieces moves pos = foldl (flip movePiece) pos moves

pColor :: Piece -> PColor
pColor (Piece _ color) = color

invert :: PColor -> PColor
invert White = Black
invert Black = White
