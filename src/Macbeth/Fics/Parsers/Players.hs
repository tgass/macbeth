{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.Players (
  players,
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

