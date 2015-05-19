{-# LANGUAGE OverloadedStrings #-}

module Api (
  GameType (..),
  Color (..),
  invert,
  Piece (..),
  pColor,
  PType (..),
  Square (..),
  Row (..),
  Column (..),
  Position ,
  removePiece,
  getPiece,
  Rating (..),
  GameResult (..),
  turnToGameResult
) where


import Data.Char (toLower)

data Color = Black | White deriving (Eq, Show)

invert :: Color -> Color
invert White = Black
invert Black = White

data Rating = Rating {r :: Int} | Unrated | Guest

instance Show Rating where
  show (Rating r') = Prelude.show r'
  show Guest = "Guest"
  show Unrated = "Unrated"

data GameType =  Blitz | Lightning | Untimed | ExaminedGame | Standard | Wild | Atomic |
                 Crazyhouse | Bughouse | Losers | Suicide | NonStandardGame  deriving (Show)

data Column = A | B | C | D | E | F | G | H deriving (Show, Enum, Ord, Eq)

data Row = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Ord, Show, Enum, Bounded)

data PType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq)

data Piece = Piece PType Color deriving (Show, Eq)

data Square = Square Column Row deriving (Ord, Eq)

instance Show Square where
  show (Square s y) = fmap toLower (show s) ++ show (fromEnum y + 1)

type Position = [(Square, Piece)]


pColor :: Piece -> Color
pColor (Piece _ _color) = _color


removePiece :: Position -> Square -> Position
removePiece pos sq = filter (\(sq', _) -> sq /= sq') pos


getPiece :: Position -> Square -> Color -> Maybe Piece
getPiece pos sq color = sq `lookup` pos >>= checkColor color
  where
    checkColor :: Color -> Piece -> Maybe Piece
    checkColor c p@(Piece _ c') = if c == c' then Just p else Nothing

turnToGameResult :: Color -> GameResult
turnToGameResult Black = WhiteWins
turnToGameResult White = BlackWins


data GameResult = WhiteWins | BlackWins | Draw

instance Show GameResult where
  show WhiteWins = "1-0"
  show BlackWins = "0-1"
  show Draw      = "1/2-1/2"

