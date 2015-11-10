{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.SeekMsgParsers (
  clearSeek,
  newSeek,
  removeSeeks
) where

import Macbeth.Api.Api
import Macbeth.Api.CommandMsg
import Macbeth.Api.Game
import Macbeth.Api.Rating
import Macbeth.Api.Seek

import Control.Applicative
import Data.Attoparsec.ByteString.Char8

clearSeek :: Parser CommandMsg
clearSeek = "<sc>" *> pure ClearSeek

newSeek :: Parser CommandMsg
newSeek = NewSeek <$> seek'

removeSeeks :: Parser CommandMsg
removeSeeks = RemoveSeeks <$> ("<sr>" *> many1 (space *> decimal))

seek' :: Parser Seek
seek' = Seek
  <$> ("<s>" *> space *> decimal)
  <*> (space *> "w=" *> manyTill anyChar space)
  <*> ("ti=" *> manyTill anyChar space *> "rt=" *> rating')
  <*> (space *> "t=" *> decimal)
  <*> (space *> "i=" *> decimal)
  <*> (space *> "r=" *> ("r" *> pure True <|> "u" *> pure False))
  <*> (space *> "tp=" *> gameType')
  <*> (space *> "c=" *> ("W" *> pure (Just White) <|>
                         "B" *> pure (Just Black) <|>
                         "?" *> pure Nothing))
  <*> (space *> "rr=" *> ((,) <$> decimal <*> ("-" *> decimal)))

gameType' :: Parser GameType
gameType' =
  "blitz" *> pure Blitz <|>
  "lightning" *> pure Lightning <|>
  "untimed" *> pure Untimed <|>
  "examined" *> pure ExaminedGame <|>
  "standard" *> pure Standard <|>
  "wild/" *> decimal *> pure Wild <|>
  "atomic" *> pure Atomic <|>
  "crazyhouse" *> pure Crazyhouse <|>
  "bughouse" *> pure Bughouse <|>
  "losers" *> pure Losers <|>
  "suicide" *> pure Suicide <|>
  takeTill (== ' ') *> pure NonStandardGame


rating' :: Parser Rating
rating' = Rating <$> decimal >>= \r -> " " *> pure r <|> "E" *> pure r <|> "P" *> pure Guest

