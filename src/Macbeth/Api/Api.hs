{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Api.Api (
  PColor (..),
  Piece (..),
  PType (..),
  Square (..),
  Row (..),
  Column (..),
  Position,
  PendingOffer (..),
  MoveDetailed (..),
  PieceMove (..),
  movePiece,
  movePieces,
  pColor,
  invert
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

data PieceMove = PieceMove { piece :: Piece, from :: Square, to :: Square }

movePiece :: PieceMove -> Position -> Position
movePiece (PieceMove piece from to) position =
  filter (\(s, _) -> s /= from && s /= to) position ++ [(to, piece)]

movePieces :: [PieceMove] -> Position -> Position
movePieces [] pos = pos
movePieces (x:xs) pos = movePieces xs (movePiece x pos)


instance Show PieceMove where
    show (PieceMove (Piece King White) (Square E One) (Square G One)) = "O-O"
    show (PieceMove (Piece King White) (Square E One) (Square C One)) = "O-O-O"
    show (PieceMove (Piece King Black) (Square E Eight) (Square G Eight)) = "O-O"
    show (PieceMove (Piece King Black) (Square E Eight) (Square C Eight)) = "O-O-O"
    show (PieceMove _ s1 s2) = show s1 ++ show s2



data PendingOffer = PendingOffer { offerId :: Int, offer :: String } deriving (Show, Eq)

data MoveDetailed = Simple Square Square | CastleLong | CastleShort deriving (Show, Eq)

pColor :: Piece -> PColor
pColor (Piece _ color) = color


invert :: PColor -> PColor
invert White = Black
invert Black = White

