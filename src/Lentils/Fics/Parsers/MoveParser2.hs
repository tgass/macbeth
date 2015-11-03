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
import Data.Maybe
import qualified Data.Attoparsec.ByteString.Char8 as A (take, )
import qualified Data.ByteString.Char8 as BS

move :: Parser Move
move = do
  pos <- BS.unpack `fmap` (takeTill (== '<') *> "<12>" *> space *> A.take 71)
  turn <- space *> ("B" *> pure Black <|> "W" *> pure White)
  doublePawnPush <- space *> ("-1" *> pure Nothing <|> liftM Just column)
  castling <- toList
                 <$> (space *> ("0" *> pure Nothing <|> "1" *> pure (Just WhiteShort)))
                 <*> (space *> ("0" *> pure Nothing <|> "1" *> pure (Just WhiteLong)))
                 <*> (space *> ("0" *> pure Nothing <|> "1" *> pure (Just BlackShort)))
                 <*> (space *> ("0" *> pure Nothing <|> "1" *> pure (Just BlackLong)))
  ply <- space *> decimal -- the number of moves made since the last irreversible move, halfmove clock
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

  return $ Move pos
                (parsePosition pos)
                turn
                doublePawnPush
                castling
                ply
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

parseRelation =
  "-3" *> pure Other <|>
  "-2" *> pure Other <|>
  "2"  *> pure Other <|>
  "-1" *> pure OponentsMove <|>
  "1"  *> pure MyMove <|>
  "0"  *> pure Observing

column :: Parser Column
column =
  "0" *> pure A <|>
  "1" *> pure B <|>
  "2" *> pure C <|>
  "3" *> pure Lentils.Api.Api.D <|>
  "4" *> pure E <|>
  "5" *> pure F <|>
  "6" *> pure G <|>
  "7" *> pure H


toList a b c d = catMaybes [a, b, c, d]
