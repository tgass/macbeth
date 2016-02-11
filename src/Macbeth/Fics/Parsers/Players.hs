{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.Players (
  finger,
  players,
  history,
  partnerNotOpen,
  players',
  player'
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Player
import Macbeth.Fics.Parsers.Api
import qualified Macbeth.Fics.Parsers.RatingParser as RP

import Control.Applicative
import Data.Attoparsec.ByteString.Char8


players :: Parser FicsMessage
players = Players <$> (commandHead 146 *> players')


partnerNotOpen :: Parser FicsMessage
partnerNotOpen = PartnerNotOpen <$> (commandHead 84 *> many1 letter_ascii <* " is not open for bughouse.")


finger :: Parser FicsMessage
finger = Finger
  <$> (commandHead 37 *> "Finger of " *> many1 letter_ascii <* ":")
  <*> manyTill anyChar "\ETB"


history :: Parser FicsMessage
history = history' <|> emptyHistory


history' :: Parser FicsMessage
history' = History
  <$> (commandHead 51 *> "\nHistory for " *> many1 letter_ascii <* ":")
  <*> manyTill anyChar "\ETB"


emptyHistory :: Parser FicsMessage
emptyHistory = do
  username <- commandHead 51 *> many1 letter_ascii <* " has no history games."
  return $ History username (username ++ " has no history games.\n")


players' :: Parser [Player]
players' = concat <$> sepBy (many' (player' <* many " ")) "\n"


player' :: Parser Player
player' = Player
  <$> RP.rating
  <*> status'
  <*> handle'


status' :: Parser Status
status' =
  "^" *> pure InvolvedInAGame <|>
  "~" *> pure RunningASimulMatch <|>
  ":" *> pure NotOpenForMatch <|>
  "#" *> pure ExaminingAGame <|>
  "." *> pure InactiveOrBusy <|>
  " " *> pure NotBusy <|>
  "&" *> pure InvolvedInATournament


handle' :: Parser Handle
handle' = Handle
  <$> many1' letter_ascii
  <*> option None ("(" *> handleType' <* ")")


handleType' :: Parser HandleType
handleType' =
  "*" *> pure Admin <|>
  "B" *> pure Blindfold <|>
  "C" *> pure Computer <|>
  "T" *> pure Team <|>
  "U" *> pure Unregistered <|>
  "CA" *> pure ChessAdvisor <|>
  "SR" *> pure ServiceRepresentative <|>
  "TD" *> pure ServiceRepresentative <|>
  "TM" *> pure MamerManager <|>
  "GM" *> pure GrandMaster <|>
  "IM" *> pure InternationalMaster <|>
  "FM" *> pure FideMaster <|>
  "WGM" *> pure WomenGrandMaster <|>
  "WIM" *> pure WomenInternationalMaster <|>
  "WFM" *> pure WomenFideMaster

