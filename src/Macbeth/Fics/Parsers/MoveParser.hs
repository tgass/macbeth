module Macbeth.Fics.Parsers.MoveParser (
  move,
  moveOnly,
  pieceHolding,
  pieceHoldingOnly,
  verboseMove'
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move hiding (relation)
import Macbeth.Fics.Message hiding (move, Observing)
import qualified Macbeth.Fics.Parsers.Api as Api
import Macbeth.Fics.Parsers.PositionParser

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (D)
import Data.Maybe
import qualified Data.Attoparsec.ByteString.Char8 as A (take)
import qualified Data.ByteString.Char8 as BS

-- https://www.freechess.org/Help/HelpFiles/style12.html

move :: Parser Move
move = "<12>" *> moveOnly

moveOnly :: Parser Move
moveOnly = do
  pos <- BS.unpack `fmap` (space *> A.take 71)
  Move
    <$> pure pos
    <*> pure (parsePosition pos)
    <*> (space *> ("B" *> pure Black <|> "W" *> pure White)) -- turn
    <*> (space *> ("-1" *> pure Nothing <|> Just `fmap` column)) -- doublePawnPush
    <*> (catMaybes <$> sequence [ castle WhiteShort, castle WhiteLong, castle BlackShort, castle BlackLong])
    <*> (space *> decimal) -- the number of moves made since the last irreversible move, halfmove clock
    <*> (space *> Api.gameId) -- gameId
    <*> (space *> manyTill anyChar space) -- nameW
    <*> manyTill anyChar space -- nameB
    <*> relation
    <*> (space *> decimal) --initialTime
    <*> (space *> decimal) --inc per move
    <*> (space *> decimal) -- whiteRelStrength
    <*> (space *> decimal) -- blackRelStrength
    <*> (space *> (decimal <|> ("-" *> (decimal >>= pure . negate)))) -- remTimeWhite
    <*> (space *> (decimal <|> ("-" *> (decimal >>= pure . negate)))) -- remTimeBlack
    <*> (space *> decimal) -- moveNumber
    <*> (space *> verboseMove') -- moveVerbose
    <*> (space *> manyTill anyChar space) -- timeTaken
    <*> ("none" *> pure Nothing <|> Just `fmap` manyTill anyChar space) -- movePretty


verboseMove' :: Parser (Maybe MoveDetailed)
verboseMove' = ("none" *> pure Nothing) <|> Just <$> (
  (Simple <$> (anyChar *> "/" *> square) <*> ("-" *> square <* takeTill (== ' '))) <|>
  (Drop <$> (anyChar *> "/@@-" *> square <* takeTill (== ' '))) <|>
  ("o-o-o" *> pure CastleLong) <|>
  ("o-o" *> pure CastleShort))


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


relation :: Parser Relation
relation =
  "-3" *> pure IsolatedPosition <|> "-2" *> pure ObservingExaminedGame <|> "2"  *> pure Examiner <|>
  "-1" *> pure OponentsMove <|> "1"  *> pure MyMove <|> "0"  *> pure Observing


column :: Parser Column
column =
  "0" *> pure A <|> "1" *> pure B <|>  "2" *> pure C <|> "3" *> pure D <|>
  "4" *> pure E <|> "5" *> pure F <|> "6" *> pure G <|> "7" *> pure H


pieceHolding :: Parser Message
pieceHolding = "<b1>" *> pieceHoldingOnly


pieceHoldingOnly :: Parser Message
pieceHoldingOnly = PieceHolding
  <$> (" game " *> Api.gameId <* " ")
  <*> ("white [" *> many' ((`Piece` White) <$> dropablePiece) <* "] ")
  <*> ("black [" *> many' ((`Piece` Black) <$> dropablePiece) <* "]" <* option "" " <-")


dropablePiece :: Parser PType
dropablePiece =
  "P" *> pure Pawn <|> "R" *> pure Rook <|> "N" *> pure Knight <|>
  "B" *> pure Bishop <|> "Q" *> pure Queen


