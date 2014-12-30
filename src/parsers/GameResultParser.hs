{-# LANGUAGE OverloadedStrings #-}

module GameResultParser (
  parseGameResult
) where

import Api
import CommandMsg

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as A (take, takeWhile)
import qualified Data.ByteString.Char8 as BS


{-
 - "{Game 368 (ALTOTAS vs. CalicoCat) CalicoCat resigns} 1-0"
 -  {Game 72 (Pehmotiikeri vs. Fischmob) Neither player has mating material} 1/2-1/2
-}

parseGameResult :: Parser CommandMsg
parseGameResult = do
  string "{Game"
  space
  gameId <- decimal
  space
  takeTill (== '}')
  char '}'
  space
  result <- (string "1-0") *> pure WhiteWins <|>
            (string "0-1") *> pure BlackWins <|>
            (string "1/2-1/2") *> pure Draw
  return $ GameResultMsg gameId result


dummy = BS.pack "{Game 368 (ALTOTAS vs. CalicoCat) CalicoCat resigns} 1-0"
