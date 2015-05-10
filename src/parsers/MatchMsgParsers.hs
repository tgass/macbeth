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
gameResult = GameResult
  <$> (skipSpace *> "{Game" *> space *> decimal)
  <*> (manyTill anyChar "} " *> "1-0" *> pure WhiteWins <|> "0-1" *> pure BlackWins <|>  "1/2-1/2" *> pure Draw)


match' :: Parser CommandMsg
match' = Match <$> ("{Game " *> decimal <* (takeTill (== ')') *> ") Creating "))


challenge :: Parser CommandMsg
challenge = Challenge
  <$> ("Challenge: " *> manyTill anyChar space)
  <*> ("(" *> rating)
  <*> (") " *> manyTill anyChar space)
  <*> ("(" *> rating)
  <*> (") " *> manyTill anyChar ".") --unrated blitz 2 12."


declinedChallenge :: Parser CommandMsg
declinedChallenge = "\"" >> manyTill anyChar "\" declines the match offer." >> return MatchDeclined


drawOffered :: Parser CommandMsg
drawOffered = manyTill anyChar space >> "offers you a draw." >> return DrawOffered



challenge' = BS.pack "Challenge: GuestYWYK (----) GuestMGSD (----) unrated blitz 2 12."
matchMsg = BS.pack "{Game 537 (GuestWSHB vs. GuestNDKP) Creating unrated blitz match.}"
gameResult' = BS.pack  "{Game 368 (ALTOTAS vs. CalicoCat) CalicoCat resigns} 1-0"
gameResult'' = BS.pack "\n{Game 406 (GuestQLHT vs. GuestVYVJ) GuestQLHT resigns} 0-1\n\nNo ratings adjustment done."
drawOffered' = BS.pack "GuestDWXY offers you a draw."
