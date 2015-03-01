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
  " ("
  manyTill anyChar space
  "vs. "
  manyTill anyChar ")"
  " Creating "
  manyTill anyChar "}"
  return $ Match id


challenge :: Parser CommandMsg
challenge = do
  "Challenge: "
  name1 <- manyTill anyChar space
  "("
  rating1 <- rating
  ") "
  name2 <- manyTill anyChar space
  "("
  rating2 <- rating
  ") "
  params <- manyTill anyChar "." --unrated blitz 2 12."
  return $ Challenge name1 rating1 name2 rating2 params

challenge' = BS.pack "Challenge: GuestYWYK (----) GuestMGSD (----) unrated blitz 2 12."
matchMsg = BS.pack "{Game 537 (GuestWSHB vs. GuestNDKP) Creating unrated blitz match.}"
gameResult' = BS.pack "{Game 368 (ALTOTAS vs. CalicoCat) CalicoCat resigns} 1-0"
