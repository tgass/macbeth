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
  Position (..),
  removePiece,
  getPiece,
  Rating (..),
  Move (..),
  remainingTime,
  decreaseRemainingTime,
  namePlayer,
  Relation (..),
  GameResult (..)
) where


data Color = Black | White deriving (Eq, Show)

invert :: Color -> Color
invert White = Black
invert Black = White

data Rating = Rating {r :: Int} | Unrated | Guest

instance Show Rating where
  show (Rating r) = show r
  show Guest = "Guest"
  show Unrated = "Unrated"

data GameType =  Blitz | Lightning | Untimed | ExaminedGame | Standard | Wild | Atomic |
                 Crazyhouse | Bughouse | Losers | Suicide | NonStandardGame  deriving (Show)

data Column = A | B | C | D | E | F | G | H deriving (Show, Enum, Ord, Eq)

data Row = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Ord, Show, Enum, Bounded)

data PType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq)

data Piece = Piece PType Color deriving (Show, Eq)

data Square = Square Column Row deriving (Show, Ord, Eq)

type Position = [(Square, Piece)]


pColor :: Piece -> Color
pColor (Piece _type _color) = _color


removePiece :: Position -> Square -> Position
removePiece pos sq = filter (\(sq', p) -> sq /= sq') pos


getPiece :: Position -> Square -> Color -> Maybe Piece
getPiece pos sq color = sq `lookup` pos >>= checkColor color
  where
    checkColor :: Color -> Piece -> Maybe Piece
    checkColor c p@(Piece _ c') = if c == c' then Just p else Nothing


data Move = Move { position :: [(Square, Piece)]
                 , turn :: Color
                 , doublePawnPush :: Maybe Int
                 , gameId :: Int
                 , nameW :: String
                 , nameB :: String
                 , relation :: Relation
                 , moveNumber :: Int
                 , moveVerbose :: String
                 , timeTaken :: String
                 , remainingTimeW :: Int
                 , remainingTimeB :: Int
                 , movePretty :: String
                 }

instance Show Move where
  show m = "Move { gameId=" ++ (show $ gameId m) ++ ", move=" ++ movePretty m ++ "}"



remainingTime :: Api.Color -> Move -> Int
remainingTime Black = remainingTimeB
remainingTime White = remainingTimeW


decreaseRemainingTime :: Api.Color -> Move -> Move
decreaseRemainingTime Black move = move {remainingTimeB = remainingTimeB move - 1}
decreaseRemainingTime White move = move {remainingTimeW = remainingTimeW move - 1}


namePlayer :: Api.Color -> Move -> String
namePlayer White = nameW
namePlayer Black = nameB



data Relation = MyMove | OponentsMove | Observing | Other deriving (Show)

data GameResult = WhiteWins | BlackWins | Draw deriving (Show)

