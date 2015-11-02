{-# LANGUAGE OverloadedStrings #-}

module Lentils.Fics.Parsers.GamesParser (
  parseGamesList
) where

import Lentils.Api.Game
import Lentils.Fics.Parsers.RatingParser

import Control.Applicative
import Data.Attoparsec.ByteString.Char8


parseGamesList :: Parser [Game]
parseGamesList = many' gameP

gameP :: Parser Game
gameP = Game
  <$> (takeTill (== '\n') *> "\n" *> many space *> decimal)
  <*> (many1 space *> option False ("(Exam." *> pure True))
  <*> (option False ("(Setup" *> pure True))
  <*> (many space *> rating)
  <*> (many1 space *> manyTill anyChar space)
  <*> (many space *> rating)
  <*> (many1 space *> manyTill anyChar (space <|> char ')'))
  <*> (takeTill (== '[') *> "[" *> settings')


settings' :: Parser GameSettings
settings' = GameSettings
  <$> (space *> pure False <|> char 'p' *> pure True)
  <*> gameType'
  <*> (char 'u' *> pure False <|> char 'r' *> pure True)


gameType' :: Parser GameType
gameType' =
  "b" *> pure Blitz <|>
  "l" *> pure Lightning <|>
  "u" *> pure Untimed <|>
  "e" *> pure ExaminedGame <|>
  "s" *> pure Standard <|>
  "w" *> pure Wild <|>
  "x" *> pure Atomic <|>
  "z" *> pure Crazyhouse <|>
  "B" *> pure Bughouse <|>
  "L" *> pure Losers <|>
  "S" *> pure Suicide <|>
  "n" *> pure NonStandardGame

