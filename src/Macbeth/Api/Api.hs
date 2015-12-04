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

data PendingOffer = PendingOffer { offerId :: Int, offer :: String } deriving (Show, Eq)

data MoveDetailed = Simple Square Square | CastleLong | CastleShort deriving (Show, Eq)

pColor :: Piece -> PColor
pColor (Piece _ color) = color


invert :: PColor -> PColor
invert White = Black
invert Black = White

