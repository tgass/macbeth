{-# LANGUAGE OverloadedStrings #-}

module SeekParser (
  parseSeeks
) where

import Seek

import Control.Applicative ((<*>), (*>), (<$>), (<|>), pure)
import Data.Attoparsec.Char8
import qualified Data.Attoparsec.Char8 as A (take)
import qualified Data.ByteString.Char8 as BS

parseSeeks :: BS.ByteString -> [Seek]
parseSeeks s = case parseOnly parseSeeks' s of
                 Left _ -> []
                 Right seek -> seek

parseSeeks' :: Parser [Seek]
parseSeeks' = many1 parseSeek'

--" 11 2324 parrot(C)          28   1 rated   standard               0-9999 f"
--" 26 2324 doctorweb(C)        5   0 rated   blitz                  0-9999 "
--" 90 1669 Brx                30   0 rated   standard               0-9999 m"
--"109 ++++ Cstreet            15  20 unrated standard               0-9999 m"
parseSeek' :: Parser Seek
parseSeek' = do
  many' space
  id <- decimal
  space
  rating <- parsePlayerRating
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
  someSpace
  many' letter_ascii -- [m, f]
  many' $ string "\n\r"
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

parsePlayerRating :: Parser Int
parsePlayerRating = decimal <|>
                    string "++++" *> pure 0 <|>
                    string "----" *> pure (-1)

parseRatingRange :: Parser (Int, Int)
parseRatingRange = decimal >>= \start ->
                   char '-' >>
                   decimal >>= \end ->
                   return (start, end)


seeks = "  1 2079 foytik              9   9 rated   standard            1960-2999 m\n\r  5 1457 LittleLurKing(C)    5   0 rated   blitz                  0-9999 f\n\r 27 1457 LittleLurKing(C)    3   0 rated   blitz                  0-9999 f\n\r 32 ++++ GuestXZHP           9  30 unrated standard               0-9999 \n\r 46 ++++ GuestGSBW          20   0 unrated standard               0-9999 f\n\r 47 1131 Suita               5  10 rated   blitz                  0-9999 \n\r 48 1338 LEEPER              3  15 rated   blitz                  0-9999 \n\r 62 1725 mscp(C)             5   0 rated   blitz                  0-9999 mf\n\r 79 1234 Dyotanaa           20   5 rated   standard   [white]     0-9999 \n\r 80 1552 pmiral              3   0 rated   blitz               1000-9999 m\n\r 87 1109 dastan              5   0 rated   blitz                  0-9999 \n\r 92 2065 foggydew(C)        20  20 rated   standard               0-9999 f\n\r 93 2329 Knightsmasher(C)   15   0 rated   standard               0-9999 f\n\r109 2192 MrsLurKing(C)      30   0 rated   standard               0-9999 f\n\r115 1827 notropis(C)         5   0 rated   blitz                  0-9999 m\n\r116 1937 GriffySr(C)         5   0 rated   blitz                  0-9999 mf\n\r137 2192 MrsLurKing(C)      20   0 rated   standard               0-9999 f\n\r140 ++++ GuestNTZV          15   5 unrated standard               0-9999 \n\r143 1409 educhancay          5   4 rated   blitz                  0-9999 \n\r144 1140 abpk                5   0 rated   blitz      [white]     0-9999 \n\r146 2118 GriffySr(C)        15   0 rated   standard               0-9999 mf\n\r152 2387 GnuCheese(C)        2  12 rated   blitz                  0-9999 mf\n\r153 2420 GnuCheese(C)       15   5 rated   standard               0-9999 mf\n\r170 1724 desantnjik         20   0 rated   standard               0-9999 \n\r173 ++++ GuestNDLS          25  25 unrated standard               0-9999 mf\n\r178 1782 mhmhmh             30  10 rated   standard            1600-9999 m\n\r185 ---- shaows             10   0 unrated blitz                  0-9999 \n\r194 2001 notropis(C)        15   0 rated   standard               0-9999 m\n\r195 ++++ GuestBXMX           5   0 unrated blitz                  1-1399 \n\r210 1127 Anandvishy          5   5 unrated blitz                  0-9999 m\n\r217 1308 nnr                15   5 rated   standard               0-9999 \n\r222 1735 callipygian(C)      5   0 rated   blitz                  0-9999 f\n\r229 1734 gambitistdownfaLL  15   0 rated   standard               0-9999 m\n\r33 ads displayed.\n\r"
