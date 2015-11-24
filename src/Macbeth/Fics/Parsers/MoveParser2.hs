{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.MoveParser2 (
  move
) where

import Macbeth.Api.Api
import Macbeth.Fics.Parsers.PositionParser
import Macbeth.Api.Move

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import qualified Data.Attoparsec.ByteString.Char8 as A (take, )
import qualified Data.ByteString.Char8 as BS

--test = BS.pack "<12> --kr-bnr ppp-pppp --nqb--- ---p---- ---P-B-- --NQ---P PPP-PPP- R---KBNR W -1 1 1 0 0 1 345 GuestTVTH GuestPYFX -1 5 0 39 39 282 288 6 o-o-o (0:03) O-O-O 1 1 0"
--test2 = BS.pack "<12> --kr-bnr ppp-pppp --nqb--- ---p---- ---P-B-- --NQ---P PPP-PPP- R---KBNR W -1 1 1 0 0 1 345 GuestTVTH GuestPYFX -1 5 0 39 39 282 288 6 o-o (0:03) O-O-O 1 1 0"

move :: Parser Move
move = do
  pos <- BS.unpack `fmap` ("<12>" *> space *> A.take 71)
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
  moveVerbose <- space *> parseVerboseMove
  timeTaken <- space *> manyTill anyChar space
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

--P/c7-c5
-- P/f2-f1=R
parseVerboseMove :: Parser (Maybe MoveDetailed)
parseVerboseMove = ("none" *> pure Nothing) <|>
  (Just `liftM` (Simple <$> (anyChar *> anyChar *> square) <*> ("-" *> square <* takeTill (== ' ')))) <|>
  (Just `liftM` ("o-o-o" *> pure CastleLong)) <|>
  (Just `liftM` ("o-o" *> pure CastleShort))


square :: Parser Square
square = Square
  <$> columnAH
  <*> row

columnAH :: Parser Column
columnAH =
  "a" *> pure A <|>
  "b" *> pure B <|>
  "c" *> pure C <|>
  "d" *> pure Macbeth.Api.Api.D <|>
  "e" *> pure E <|>
  "f" *> pure F <|>
  "g" *> pure G <|>
  "h" *> pure H

row :: Parser Row
row =
  "1" *> pure One <|>
  "2" *> pure Two <|>
  "3" *> pure Three <|>
  "4" *> pure Four <|>
  "5" *> pure Five <|>
  "6" *> pure Six <|>
  "7" *> pure Seven <|>
  "8" *> pure Eight

parseRelation =
  "-3" *> pure IsolatedPosition <|>
  "-2" *> pure ObservingExaminedGame <|>
  "2"  *> pure Examiner <|>
  "-1" *> pure OponentsMove <|>
  "1"  *> pure MyMove <|>
  "0"  *> pure Observing

column :: Parser Column
column =
  "0" *> pure A <|>
  "1" *> pure B <|>
  "2" *> pure C <|>
  "3" *> pure Macbeth.Api.Api.D <|>
  "4" *> pure E <|>
  "5" *> pure F <|>
  "6" *> pure G <|>
  "7" *> pure H


toList a b c d = catMaybes [a, b, c, d]
