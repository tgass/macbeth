{-# LANGUAGE OverloadedStrings #-}

module MatchMsgParsers (
  gameResult,
  match',
  challenge
) where

import Api
import CommandMsg
import ParseUtils

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as A (take, takeWhile)
import qualified Data.ByteString.Char8 as BS


{-
 - "{Game 368 (ALTOTAS vs. CalicoCat) CalicoCat resigns} 1-0"
 -  {Game 72 (Pehmotiikeri vs. Fischmob) Neither player has mating material} 1/2-1/2
-}

gameResult :: Parser CommandMsg
gameResult = do
  "{Game"
  space
  gameId <- decimal
  space
  takeTill (== '}')
  char '}'
  space
  result <- "1-0" *> pure WhiteWins <|>
            "0-1" *> pure BlackWins <|>
            "1/2-1/2" *> pure Draw
  return $ GameResult gameId result


match' :: Parser CommandMsg
match' = do
  "{Game "
  id <- decimal
  "("
  manyTill anyChar space
  " vs. "
  manyTill anyChar space
  ") Creating "
  manyTill anyChar "}"
  return $ Match id


challenge :: Parser CommandMsg
challenge = do
  "Challenge: "
  name1 <- manyTill anyChar space
  space
  "("
  rating1 <- rating
  ") "
  name2 <- manyTill anyChar space
  space
  "("
  rating2 <- rating
  ") "
  params <- manyTill anyChar "." --unrated blitz 2 12."
  return $ Challenge name1 rating1 name2 rating2 params


matchMsg = BS.pack "{Game 537 (GuestWSHB vs. GuestNDKP) Creating unrated blitz match.}"
dummy = BS.pack "{Game 368 (ALTOTAS vs. CalicoCat) CalicoCat resigns} 1-0"
