{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Api.Api (
  PColor (..),
  Piece (..),
  PType (..),
  Square (..),
  Row (..),
  Column (..),
  Position,
  diffPosition,
  PieceMove (..),
  movePiece,
  movePieces,
  PendingOffer (..),
  MoveDetailed (..),
  pColor,
  invert
) where

import Data.Char
import Data.List

data Column = A | B | C | D | E | F | G | H deriving (Show, Enum, Ord, Eq)

data Row = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Ord, Show, Enum, Bounded)

data PType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq)

data PColor = Black | White deriving (Eq, Show)

data Piece = Piece PType PColor deriving (Show, Eq)

data Square = Square Column Row deriving (Ord, Eq)

instance Show Square where
  show (Square s y) = fmap toLower (show s) ++ show (fromEnum y + 1)

type Position = [(Square, Piece)]

diffPosition :: Position -> Position -> [PieceMove]
diffPosition before after = let from = before \\ after
                                to = after \\ before
                            in [PieceMove piece1 s1 s2 | (s1, piece1) <- from, (s2, piece2) <- to
                                                    , piece1 == piece2
                                                    , s1 /= s2 ]


data PieceMove = PieceMove { piece :: Piece, from :: Square, to :: Square }

instance Show PieceMove where
  show (PieceMove _ s1 s2) = show s1 ++ show s2

movePiece :: PieceMove -> Position -> Position
movePiece (PieceMove piece from to) position =
  filter (\(s, _) -> s /= from && s /= to) position ++ [(to, piece)]

movePieces :: [PieceMove] -> Position -> Position
movePieces [] pos = pos
movePieces (x:xs) pos = movePieces xs (movePiece x pos)

data PendingOffer = PendingOffer { offerId :: Int, offer :: String } deriving (Show, Eq)

data MoveDetailed = Simple Square Square | CastleLong | CastleShort deriving (Show, Eq)

pColor :: Piece -> PColor
pColor (Piece _ color) = color


invert :: PColor -> PColor
invert White = Black
invert Black = White

