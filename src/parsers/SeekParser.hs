{-# LANGUAGE OverloadedStrings #-}

module SeekParser (
  soughtList'
) where

import Api
import Seek

import Control.Applicative ((*>), (<|>), pure)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS


soughtList' :: Parser [Seek]
soughtList' = manyTill parseSeek' lastLine
                where lastLine = do
                                count <- decimal
                                space
                                "ads displayed.\n"


--" 11 2324 parrot(C)          28   1 rated   standard               0-9999 f"
--" 26 2324 doctorweb(C)        5   0 rated   blitz                  0-9999 "
--" 90 1669 Brx                30   0 rated   standard               0-9999 m"
--"109 ++++ Cstreet            15  20 unrated standard               0-9999 m"
parseSeek' :: Parser Seek
parseSeek' = do
  many' space
  id <- decimal
  s1
  rating <- parseRating
  s1
  name <- manyTill anyChar space
  s1
  timeStart <- decimal
  s1
  timeIncPerMove <- decimal
  s1
  isRated <- parseIsRated
  s1
  gameType <- parseGameType
  s1
  color <- parseColor'
  many' space
  ratingRange <- parseRatingRange
  space
  option "" "m"
  option "" "f"
  "\n"
  return $ Seek id rating name timeStart timeIncPerMove isRated gameType color ratingRange

s1 :: Parser String
s1 = many1 space

parseGameType :: Parser GameType
parseGameType = "untimed" *> pure Untimed <|>
                "lightning" *> pure Lightning <|>
                "blitz" *> pure Blitz <|>
                "standard" *> pure Standard <|>
                manyTill anyChar space *> pure NonStandardGame

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

seeks1 = BS.pack " 12 ++++ GuestRTH            2  12 unrated blitz                  0-9999 \n 26 ++++ GuestLLMM          10   5 unrated blitz                  0-9999 f\n 38 ++++ GuestWBMC          10   0 unrated blitz                  0-1999 m\n 49 2598 masheen(C)          5   0 unrated suicide                0-9999 \n 73 ++++ GuestNQRN           5   0 unrated blitz                  0-9999 f\n 83 2598 masheen(C)          2  12 unrated suicide                0-9999 \n 96 ++++ PhineasFogg        15  20 unrated standard               0-9999 m\n167 1991 CatNail(C)          3   0 unrated suicide                0-9999 m\n8 ads displayed.\n"

seeks = BS.pack "  8 1237 FreoDockers        10   0 rated   blitz                  0-9999 f\n  9 1993 GriffyJr(C)        15   0 rated   standard               0-9999 mf\n 12 ---- Lesdat             15   0 unrated standard               0-9999 \n 17 2280 Sillycon(C)        15   0 rated   standard               0-9999 f\n 20 1415 Tchessgood          3   1 rated   blitz                  0-9999 \n 31 2391 GnuCheese(C)        2  12 rated   blitz                  0-9999 mf\n 41 1257 sehgal             15   5 rated   standard               0-9999 \n 42 ++++ albuferas           3   0 unrated blitz                  0-9999 m\n 43 ++++ GuestCPYP           5   5 unrated blitz      [black]     0-9999 f\n 44 2130 MrsLurKing(C)      30   0 rated   standard               0-9999 f\n 46 1970 GriffySr(C)         5   0 rated   blitz                  0-9999 mf\n 47 2387 GnuCheese(C)       15   5 rated   standard               0-9999 mf\n 53 2064 GriffySr(C)        15   0 rated   standard               0-9999 mf\n 54 1916 IFDThor(C)          5   0 rated   blitz                  0-9999 f\n 72 1988 JuniorLurKing(C)   15   0 rated   standard               0-9999 f\n 86 2130 MrsLurKing(C)      20   0 rated   standard               0-9999 f\n 97 1401 ButchFantasy        5   2 rated   blitz                  0-9999 \n 99 1930 GriffyJr(C)         5   0 rated   blitz                  0-9999 mf\n111 1112 elusivity           3   0 rated   blitz                  0-9999 \n112 ++++ GuestJNFM           3   0 unrated blitz                  0-9999 \n165 2278 Knightsmasher(C)   15   0 rated   standard               0-9999 f\n166 ++++ GuestFFFQ           8  30 unrated standard   [white]     0-9999 m\n22 ads displayed.\n"
--seeks2 = BS.pack " 13 1148 Salaspils           5   5 rated   blitz                  0-9999 \n 57  958 pachequin          10   0 rated   blitz                  0-9999 \n 64 ++++ GuestHTBQ          10   0 unrated blitz                  0-9999 \n 65 ++++ GuestGSCT          15   5 unrated standard   [black]     0-9999 \n 66 2381 GnuCheese(C)        2  12 rated   blitz                  0-9999 mf\n 70 1956 GriffySr(C)         5   0 rated   blitz                  0-9999 mf\n 71 1334 bamboorama          2  12 unrated blitz                  0-9999 \n 76 2387 GnuCheese(C)       15   5 rated   standard               0-9999 mf\n 79 1307 AttilaM             3   2 rated   blitz                  0-1550 \n100 2281 Sillycon(C)        15   0 rated   standard               0-9999 f\n102 2068 GriffySr(C)        15   0 rated   standard               0-9999 mf\n103 ++++ GuestLPZS           5   5 unrated blitz      [white]     0-9999 \n112 ++++ pikutrex            5   2 unrated blitz                  0-3000 \n115 ++++ GuestYWLZ          10   0 unrated blitz                  0-9999 mf\n116 1521 isavich            80   0 rated   standard   [white]     0-9999 \n121 1379 Kallehas            5   0 unrated blitz                  0-1500 m\n122 1858 IFDThor(C)          5   0 rated   blitz                  0-9999 f\n123 ++++ GuestTBLV           5  10 unrated blitz      [white]     0-9999 \n151 2285 Knightsmasher(C)   15   0 rated   standard               0-9999 f\n159 2037 foggydew(C)        20  20 rated   standard               0-9999 f\n20 ads displayed.\n"

