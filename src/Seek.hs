{-# LANGUAGE OverloadedStrings #-}

module Seek (
  GameType (..),
  Color (..),
  Piece (..),
  PType (..),
  Square (..),
  Position (..),
  Row (..),
  Column (..),
  Move (..),
  Rating (..)
) where


data Color = Black | White deriving (Eq, Show)

data Rating = Rating {r :: Int} | Unrated | Guest deriving (Show)

data GameType =  Blitz | Lightning | Untimed | ExaminedGame | Standard | Wild | Atomic |
                 Crazyhouse | Bughouse | Losers | Suicide | NonStandardGame  deriving (Show)

data Column = A | B | C | D | E | F | G | H deriving (Show, Enum)

data Row = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Ord, Show, Enum, Bounded)

data PType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show)

data Piece = Piece PType Color deriving (Show)

data Square = Square Column Row deriving (Show)

type Position = [(Square, Piece)]

data Move = Move Position deriving (Show)

