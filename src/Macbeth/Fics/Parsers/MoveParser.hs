{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.MoveParser (
  move,
  pieceHolding
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.CommandMsg hiding (move)
import Macbeth.Fics.Parsers.PositionParser
import Macbeth.Fics.Api.Move hiding (relation)

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 hiding (D)
import Data.Maybe
import qualified Data.Attoparsec.ByteString.Char8 as A (take, )
import qualified Data.ByteString.Char8 as BS

--test = BS.pack "<12> --kr-bnr ppp-pppp --nqb--- ---p---- ---P-B-- --NQ---P PPP-PPP- R---KBNR W -1 1 1 0 0 1 345 GuestTVTH GuestPYFX -1 5 0 39 39 282 288 6 o-o-o (0:03) O-O-O 1 1 0"
--test2 = BS.pack "<12> --kr-bnr ppp-pppp --nqb--- ---p---- ---P-B-- --NQ---P PPP-PPP- R---KBNR W -1 1 1 0 0 1 345 GuestTVTH GuestPYFX -1 5 0 39 39 282 288 6 o-o (0:03) O-O-O 1 1 0"

move :: Parser Move
move = do
  pos <- BS.unpack `fmap` ("<12>" *> space *> A.take 71)
  Move
    <$> pure pos
    <*> pure (parsePosition pos)
    <*> (space *> ("B" *> pure Black <|> "W" *> pure White)) -- turn
    <*> (space *> ("-1" *> pure Nothing <|> liftM Just column)) -- doublePawnPush
    <*> (catMaybes <$> sequence [ castle WhiteShort, castle WhiteLong, castle BlackShort, castle BlackLong])
    <*> (space *> decimal) -- the number of moves made since the last irreversible move, halfmove clock
    <*> (space *> decimal) -- gameId
    <*> (space *> manyTill anyChar space) -- nameW
    <*> (manyTill anyChar space) -- nameB
    <*> relation --relation
    <*> (space *> decimal) --initialTime
    <*> (space *> decimal) --inc per move
    <*> (space *> decimal) -- whiteRelStrength
    <*> (space *> decimal) -- blackRelStrength
    <*> (space *> (decimal <|> ("-" *> (decimal >>= pure . negate)))) -- remTimeWhite
    <*> (space *> (decimal <|> ("-" *> (decimal >>= pure . negate)))) -- remTimeBlack
    <*> (space *> decimal) -- moveNumber
    <*> (space *> parseVerboseMove) -- moveVerbose
    <*> (space *> manyTill anyChar space) -- timeTaken
    <*> ("none" *> pure Nothing <|> liftM Just (manyTill anyChar space)) -- movePretty

--P/c7-c5
-- P/f2-f1=R
parseVerboseMove :: Parser (Maybe MoveDetailed)
parseVerboseMove = ("none" *> pure Nothing) <|>
  (Just `liftM` (Simple <$> (anyChar *> anyChar *> square) <*> ("-" *> square <* takeTill (== ' ')))) <|>
  (Just `liftM` ("o-o-o" *> pure CastleLong)) <|>
  (Just `liftM` ("o-o" *> pure CastleShort))

castle :: Castling -> Parser (Maybe Castling)
castle c = space *> ("0" *> pure Nothing <|> "1" *> pure (Just c))

square :: Parser Square
square = Square <$> columnAH <*> row


columnAH :: Parser Column
columnAH =
  "a" *> pure A <|> "b" *> pure B <|> "c" *> pure C <|> "d" *> pure D <|>
  "e" *> pure E <|> "f" *> pure F <|> "g" *> pure G <|> "h" *> pure H


row :: Parser Row
row =
  "1" *> pure One <|> "2" *> pure Two <|> "3" *> pure Three <|> "4" *> pure Four <|>
  "5" *> pure Five <|> "6" *> pure Six <|> "7" *> pure Seven <|> "8" *> pure Eight


relation =
  "-3" *> pure IsolatedPosition <|> "-2" *> pure ObservingExaminedGame <|> "2"  *> pure Examiner <|>
  "-1" *> pure OponentsMove <|> "1"  *> pure MyMove <|> "0"  *> pure Observing


column :: Parser Column
column =
  "0" *> pure A <|> "1" *> pure B <|>  "2" *> pure C <|> "3" *> pure D <|>
  "4" *> pure E <|> "5" *> pure F <|> "6" *> pure G <|> "7" *> pure H


-- <b1> game 455 white [PP] black []
pieceHolding :: Parser CommandMsg
pieceHolding = PieceHolding
  <$> ("<b1> game " *> decimal <* " ")
  <*> ("white [" *> many' dropablePiece <* "] ")
  <*> ("black [" *> many' dropablePiece <* "]" <* option "" " <-")


dropablePiece :: Parser PType
dropablePiece =
  "P" *> pure Pawn <|> "R" *> pure Rook <|> "N" *> pure Knight <|>
  "B" *> pure Bishop <|> "Q" *> pure Queen


