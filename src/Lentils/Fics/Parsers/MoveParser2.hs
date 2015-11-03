{-# LANGUAGE OverloadedStrings #-}

module Lentils.Fics.Parsers.MoveParser2 (
  move
) where

import Lentils.Api.Api
import Lentils.Fics.Parsers.PositionParser
import Lentils.Api.Move

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as A (take, )
import qualified Data.ByteString.Char8 as BS

move :: Parser Move
move = do
  pos <- takeTill (== '<') *> "<12>" *> space *> A.take 71
  turn <- space *> ("B" *> pure Black <|> "W" *> pure White)
  doublePawnPush <- space *> ("-1" *> pure Nothing <|> liftM Just decimal)
  space *> parseBool -- castle white short
  space *> parseBool -- castle white long
  space *> parseBool -- castle black short
  space *> parseBool -- castle black long
  space *> decimal -- the number of moves made since the last irreversible move
  gameId <- space *> decimal
  nameW <- space *> manyTill anyChar space
  nameB <- manyTill anyChar space
  rel <- parseRelation
  space *> decimal -- initial time
  space *> decimal -- inc per move
  space *> decimal -- white rel strength
  space *> decimal -- black rel strength
  remTimeWhite <- space *> (decimal <|> ("-" *> (decimal >>= pure . negate)))
  remTimeBlack <- space *> (decimal <|> ("-" *> (decimal >>= pure . negate)))
  moveNumber <- space *> decimal
  moveVerbose <- space *> manyTill anyChar space
  timeTaken <- manyTill anyChar space
  movePretty <- "none" *> pure Nothing <|> liftM Just (manyTill anyChar space)

  return $ Move (parsePosition (BS.unpack pos))
                turn
                doublePawnPush
                gameId
                nameW
                nameB
                rel
                moveNumber
                moveVerbose
                timeTaken
                remTimeWhite
                remTimeBlack
                movePretty

parseBool = "1" *> pure True <|> "0" *> pure False

parseRelation =
  "-3" *> pure Other <|>
  "-2" *> pure Other <|>
  "2"  *> pure Other <|>
  "-1" *> pure OponentsMove <|>
  "1"  *> pure MyMove <|>
  "0"  *> pure Observing
