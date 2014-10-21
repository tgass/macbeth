{-# LANGUAGE OverloadedStrings #-}

module Seek (
  Seek (..),
  GameType (..),
  Color (..),
  Piece (..),
  PType (..),
  Square (..),
  Position (..),
  Row (..),
  Column (..),
  Move (..),
  parseSeek,
  parsePosition,
  parseMove
) where

import Control.Applicative ((<*>), (*>), (<$>), (<|>), pure)
import Data.Attoparsec.Char8
import qualified Data.Attoparsec.Char8 as A (take)
import qualified Data.ByteString.Char8 as B
import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)

data Color = Black | White deriving (Eq, Show)

data GameType = Untimed | Lightning | Blitz | Standard deriving (Show)

data Column = A | B | C | D | E | F | G | H deriving (Show, Enum)

data Row = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Ord, Show, Enum, Bounded)

data PType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show)

data Piece = Piece PType Color deriving (Show)

data Square = Square Column Row deriving (Show)

type Position = [(Square, Piece)]

data Move = Move Position deriving (Show)

data Seek = Seek { id :: Int
                 , rating :: Int
                 , name :: String
                 , gameType :: GameType
                 , timeStart :: Int
                 , timeIncPerMove :: Int
                 , isRated :: Bool
                 , color :: Maybe Color
                 , ratingRange :: (Int, Int)
--                 , autoStart :: Bool
} deriving (Show)

parseSeek :: B.ByteString -> Maybe Seek
parseSeek s = case parseOnly parseSeek' s of
                 Left _ -> Nothing
                 Right seek -> Just seek

--" 11 2324 parrot(C)          28   1 rated   standard               0-9999 f"
--" 26 2324 doctorweb(C)        5   0 rated   blitz                  0-9999 "
--" 90 1669 Brx                30   0 rated   standard               0-9999 m"
--"109 ++++ Cstreet            15  20 unrated standard               0-9999 m"
parseSeek' :: Parser Seek
parseSeek' = do
  many' space
  id <- decimal
  space
  rating <- decimal
  space
  name <- manyTill anyChar space
  someSpace
  timeStart <- decimal
  someSpace
  timeIncPerMove <- decimal
  someSpace
  isRated <- parseIsRated
  someSpace
  gameType <- parseGameType
  someSpace
  color <- parseColor'
  many' space
  ratingRange <- parseRatingRange

  return $ Seek id rating name gameType timeStart timeIncPerMove isRated color ratingRange

someSpace :: Parser String
someSpace = many1 space

parseGameType :: Parser GameType
parseGameType = string "untimed" *> pure Untimed <|>
                string "lightning" *> pure Lightning <|>
                string "blitz" *> pure Blitz <|>
                string "standard" *> pure Standard

parseIsRated :: Parser Bool
parseIsRated = string "rated" *> pure True <|>
               string "unrated" *> pure False

parseColor' :: Parser (Maybe Color)
parseColor' = option Nothing parseColor
                where parseColor = string "[white]" *> pure (Just White) <|>
                                   string "[black]" *> pure (Just Black)

parseRatingRange :: Parser (Int, Int)
parseRatingRange = decimal >>= \start ->
                   char '-' >>
                   decimal >>= \end ->
                   return (start, end)

parsePosition :: String -> [(Square, Piece)]
parsePosition str = fmap (\(s,p) -> (s, fromJust p)) $ filter (\(s,p) -> isJust p) squares
                where rows = parseRows str
                      squares = concat $ fmap parseSquares rows

parseRows :: String -> [(Row, String)]
parseRows str = zip rows lines
             where rows = reverse [r | r <- [One .. Eight]]
                   lines = splitOn " " str

parseColumn :: String -> [(Column, Maybe Piece)]
parseColumn line = zip [c | c <- [A .. H]] [readPiece c | c <- line]

parseSquares :: (Row, String) -> [(Square, Maybe Piece)]
parseSquares (r, line) = fmap (\cc -> ((Square (fst cc) r ), snd cc)) (parseColumn line)


readPiece :: Char -> Maybe Piece
readPiece 'P' = Just(Piece Pawn White)
readPiece 'R' = Just(Piece Rook White)
readPiece 'N' = Just(Piece Knight White)
readPiece 'B' = Just(Piece Bishop White)
readPiece 'Q' = Just(Piece Queen White)
readPiece 'K' = Just(Piece King White)
readPiece 'p' = Just(Piece Pawn Black)
readPiece 'r' = Just(Piece Rook Black)
readPiece 'n' = Just(Piece Knight Black)
readPiece 'b' = Just(Piece Bishop Black)
readPiece 'q' = Just(Piece Queen Black)
readPiece 'k' = Just(Piece King Black)
readPiece _ = Nothing


move = "<12> ———— ———— ——p—— ———— ———— ——k-K ———— ———q W -1 0 0 0 0 4 203 zerowin Hutnik 0 1 0 0 10 13 26 52 Q/e1-h1 (0:00) Qh1# 0 1 818"

parseMove :: B.ByteString -> Maybe Move
parseMove s = case parseOnly parseMove' s of
              Left _ -> Nothing
              Right m -> Just m

parseMove' :: Parser Move
parseMove' = do
  string "<12>"
  space
  p <- A.take 71
  return $ Move (parsePosition $ B.unpack p)


