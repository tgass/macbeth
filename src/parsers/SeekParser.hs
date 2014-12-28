{-# LANGUAGE OverloadedStrings #-}

module SeekParser (
  parseSeeks,
  Seek (..)
) where

import Seek

import Control.Applicative ((<*>), (*>), (<|>), pure)
import Data.Attoparsec.Char8
import qualified Data.Attoparsec.Char8 as A (take)
import qualified Data.ByteString.Char8 as BS

data Seek = Seek { id :: Int
                 , rating :: Rating
                 , name :: String
                 , timeStart :: Int
                 , timeIncPerMove :: Int
                 , isRated :: Bool
                 , gameType :: GameType
                 , color :: Maybe Color
                 , ratingRange :: (Int, Int)
--                 , startMode :: StartMode
--                 , checkFormula :: Bool
} deriving (Show)

data StartMode = Auto | Manual deriving (Show)


parseSeeks :: BS.ByteString -> [Seek]
parseSeeks s = case parseOnly parseSeeks' s of
                 Left _ -> []
                 Right seek -> seek

parseSeeks' :: Parser [Seek]
parseSeeks' = manyTill parseSeek' lastLine
              where
                lastLine = do
                  count <- decimal
                  space
                  "ads displayed."


--" 11 2324 parrot(C)          28   1 rated   standard               0-9999 f"
--" 26 2324 doctorweb(C)        5   0 rated   blitz                  0-9999 "
--" 90 1669 Brx                30   0 rated   standard               0-9999 m"
--"109 ++++ Cstreet            15  20 unrated standard               0-9999 m"
parseSeek' :: Parser Seek
parseSeek' = do
  many' space
  id <- decimal
  space
  rating <- parseRating
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
  space
  option "" "m"
  option "" "f"
  return $ Seek id rating name timeStart timeIncPerMove isRated gameType color ratingRange

someSpace :: Parser String
someSpace = many1 space

parseGameType :: Parser GameType
parseGameType = "untimed" *> pure Untimed <|>
                "lightning" *> pure Lightning <|>
                "blitz" *> pure Blitz <|>
                "standard" *> pure Standard

parseIsRated :: Parser Bool
parseIsRated = "rated" *> pure True <|>
               "unrated" *> pure False

parseColor' :: Parser (Maybe Color)
parseColor' = option Nothing $ "[white]" *> pure (Just White) <|>
                               "[black]" *> pure (Just Black)

parseRating :: Parser Rating
parseRating = (decimal >>= \r -> return $ Rating r) <|>
               "++++" *> pure Guest <|>
               "----" *> pure Unrated


parseRatingRange :: Parser (Int, Int)
parseRatingRange = decimal >>= \start ->
                   char '-' >>
                   decimal >>= \end ->
                   return (start, end)


seeks3 = BS.pack "74 ++++ GuestKKJK           4   4 unrated blitz                  0-9999 11 ads displayed."
seeks1 = BS.pack " 5 ++++ GuestPLCY          10   5 unrated blitz      [black]     0-9999 "
seeks = BS.pack "43 ++++ GuestRDNJ           2  12 unrated blitz                  0-9999 \n 44 ++++ GuestSDHV          60   5 unrated standard               0-9999 \n 48 1598 snae               15   0 rated   standard               0-9999 \n 52 1393 Giannezi            9   0 unrated blitz                  0-9999 \n 53 1229 kamilia             4   0 rated   blitz                  0-9999 \n 73 ++++ GuestMZHB          20   0 unrated standard               0-9999 f\n 74 ++++ GuestKKJK           4   4 unrated blitz                  0-9999 \n 79 1356 ProfLuelo           5   0 rated   blitz      [white]     0-9999 \n 82 1276 andresilve          1   5 rated   blitz                  0-9999 \n 88 2126 MrsLurKing(C)      30   0 rated   standard               0-9999 f\n 93 1420 Yersinia            1   8 rated   blitz                  0-9999 \n 96 1789 FatGeorge          15   0 rated   standard            1840-4000 f\n 98 1633 mscp(C)             5   0 rated   blitz                  0-9999 mf\n104 2126 MrsLurKing(C)      20   0 rated   standard               0-9999 f\n119 1479 Raisha              5   4 rated   blitz                  0-9999 \n145 1479 Raisha              5   4 rated   blitz      [black]     0-9999 \n149 1473 Tewt               15   5 rated   standard   [white]     0-9999 \n151 1494 xxxupendra         15   5 rated   standard   [white]     0-9999 \n152 1875 Kismate            15  10 unrated standard            1900-9999 \n157 2718 badbehavior(C)      3   0 rated   blitz                  0-9999 f\n165 1746 VeryNewPlayer       2   6 rated   blitz               1000-2400 \n166 ++++ GuestZYWY          15   5 unrated standard   [black]     0-9999 \n174 2220 meru(C)             5   0 rated   blitz                  0-9999 \n175 ++++ GuestCNQS           5   5 unrated blitz                  0-9999 \n183 1955 GriffySr(C)         5   0 rated   blitz                  0-9999 mf\n184 2102 GriffySr(C)        15   0 rated   standard               0-9999 mf\n187 1420 Yersinia            2  12 rated   blitz                  0-9999 \n190 1211 azulao             10   0 rated   blitz                  0-9999 f\n194 2272 meru(C)            15  15 rated   standard               0-9999 \n195 ++++ GuestFQJR           5   3 unrated blitz      [white]     0-9999 \n196 ++++ GuestZDMJ          10   0 unrated blitz                  0-9999 m\n197 2718 badbehavior(C)      5   0 rated   blitz                  0-9999 f\n198 1530 onkel               5   0 rated   blitz                  0-9999 \n34 ads displayed.\n"
seeks4 = BS.pack "197 2718 badbehavior(C)      5   0 rated   blitz                  0-9999 \n197 2718 badbehavior(C)      5   0 rated   blitz                  0-9999 f\n198 1530 onkel               5   0 rated   blitz                  0-9999 m\n34 ads displayed.\n"
