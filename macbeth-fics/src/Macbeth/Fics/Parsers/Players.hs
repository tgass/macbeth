module Macbeth.Fics.Parsers.Players (
  finger,
  players,
  history,
  userHandle,
  partnerNotOpen,
  partnerOffer,
  partnerDeclined,
  partnerAccepted,
  players',
  player'
) where

import Macbeth.Fics.Message
import Macbeth.Fics.Api.Player
import Macbeth.Fics.Parsers.Api
import qualified Macbeth.Fics.Parsers.RatingParser as RP

import Control.Applicative
import Data.Attoparsec.ByteString.Char8


players :: Parser Message
players = Players <$> (commandHead 146 *> players')

partnerNotOpen :: Parser Message
partnerNotOpen = PartnerNotOpen <$> (commandHead 84 *> userHandle <* " is not open for bughouse.")

partnerOffer :: Parser Message
partnerOffer = PartnerOffer <$> (userHandle <* " offers to be your bughouse partner")

partnerDeclined :: Parser Message
partnerDeclined = PartnerDeclined <$> (userHandle <* " declines the partnership request.")

partnerAccepted :: Parser Message
partnerAccepted = PartnerAccepted <$> (userHandle <* " agrees to be your partner.")

finger :: Parser Message
finger = Finger
  <$> (commandHead 37 *> "Finger of " *> userHandle <* ":")
  <*> manyTill anyChar "\ETB"


history :: Parser Message
history = history' <|> emptyHistory


history' :: Parser Message
history' = History
  <$> (commandHead 51 *> "\nHistory for " *> userHandle <* ":")
  <*> manyTill anyChar "\ETB"


emptyHistory :: Parser Message
emptyHistory = do
  userHandle' <- commandHead 51 *> userHandle <* " has no history games."
  return $ History userHandle' (name userHandle' ++ " has no history games.\n")


players' :: Parser [Player]
players' = concat <$> sepBy (many' (player' <* many " ")) "\n"


player' :: Parser Player
player' = Player
  <$> RP.rating
  <*> status'
  <*> userHandle


status' :: Parser Status
status' =
  "^" *> pure InvolvedInAGame <|>
  "~" *> pure RunningASimulMatch <|>
  ":" *> pure NotOpenForMatch <|>
  "#" *> pure ExaminingAGame <|>
  "." *> pure InactiveOrBusy <|>
  " " *> pure NotBusy <|>
  "&" *> pure InvolvedInATournament


userHandle :: Parser UserHandle
userHandle = UserHandle
  <$> many1' letter_ascii
  <*> option [] (many handleType')


handleType' :: Parser HandleType
handleType' =
  "(*)" *> pure Admin <|>
  "(B)" *> pure Blindfold <|>
  "(C)" *> pure Computer <|>
  "(D)" *> pure NOT_DOCUMENTED <|>
  "(T)" *> pure Team <|>
  "(U)" *> pure Unregistered <|>
  "(CA)" *> pure ChessAdvisor <|>
  "(SR)" *> pure ServiceRepresentative <|>
  "(TD)" *> pure ServiceRepresentative <|>
  "(TM)" *> pure MamerManager <|>
  "(GM)" *> pure GrandMaster <|>
  "(IM)" *> pure InternationalMaster <|>
  "(FM)" *> pure FideMaster <|>
  "(WGM)" *> pure WomenGrandMaster <|>
  "(WIM)" *> pure WomenInternationalMaster <|>
  "(WFM)" *> pure WomenFideMaster

