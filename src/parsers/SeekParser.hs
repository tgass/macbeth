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
                  "ads displayed.\n"


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
  "\n"
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


seeks = BS.pack "  8 1237 FreoDockers        10   0 rated   blitz                  0-9999 f\n  9 1993 GriffyJr(C)        15   0 rated   standard               0-9999 mf\n 12 ---- Lesdat             15   0 unrated standard               0-9999 \n 17 2280 Sillycon(C)        15   0 rated   standard               0-9999 f\n 20 1415 Tchessgood          3   1 rated   blitz                  0-9999 \n 31 2391 GnuCheese(C)        2  12 rated   blitz                  0-9999 mf\n 41 1257 sehgal             15   5 rated   standard               0-9999 \n 42 ++++ albuferas           3   0 unrated blitz                  0-9999 m\n 43 ++++ GuestCPYP           5   5 unrated blitz      [black]     0-9999 f\n 44 2130 MrsLurKing(C)      30   0 rated   standard               0-9999 f\n 46 1970 GriffySr(C)         5   0 rated   blitz                  0-9999 mf\n 47 2387 GnuCheese(C)       15   5 rated   standard               0-9999 mf\n 53 2064 GriffySr(C)        15   0 rated   standard               0-9999 mf\n 54 1916 IFDThor(C)          5   0 rated   blitz                  0-9999 f\n 72 1988 JuniorLurKing(C)   15   0 rated   standard               0-9999 f\n 86 2130 MrsLurKing(C)      20   0 rated   standard               0-9999 f\n 97 1401 ButchFantasy        5   2 rated   blitz                  0-9999 \n 99 1930 GriffyJr(C)         5   0 rated   blitz                  0-9999 mf\n111 1112 elusivity           3   0 rated   blitz                  0-9999 \n112 ++++ GuestJNFM           3   0 unrated blitz                  0-9999 \n165 2278 Knightsmasher(C)   15   0 rated   standard               0-9999 f\n166 ++++ GuestFFFQ           8  30 unrated standard   [white]     0-9999 m\n22 ads displayed.\n"
