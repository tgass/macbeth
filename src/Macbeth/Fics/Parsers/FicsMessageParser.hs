{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.FicsMessageParser (
 parseFicsMessage
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Challenge
import Macbeth.Fics.FicsMessage hiding (move)
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.PendingOffer
import Macbeth.Fics.Parsers.Api
import Macbeth.Fics.Parsers.GamesParser
import Macbeth.Fics.Parsers.MoveParser
import Macbeth.Fics.Parsers.RatingParser
import qualified Macbeth.Fics.Parsers.SeekMsgParsers as SP

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Char
import Data.List.Split (splitOn)
import qualified Data.ByteString.Char8 as BS



parseFicsMessage :: BS.ByteString -> Either String FicsMessage
parseFicsMessage = parseOnly parser where
  parser = choice [ SP.clearSeek
                  , SP.newSeek
                  , SP.removeSeeks
                  , SP.seekNotAvailable
                  , SP.seekInfoBlock

                  , gameMove
                  , confirmGameMove
                  , acceptMatchOffer
                  , gameCreation
                  , seek
                  , pieceHolding

                  , challenge
                  , matchDeclined
                  , matchUserNotLoggedIn
                  , matchOfferIdentical

                  , games
                  , observe
                  , noSuchGame

                  , abortRequest
                  , abortRequestDeclined

                  , takebackRequest
                  , takebackAccepted
                  , acceptTakeback

                  , drawRequest
                  , drawRequestDeclined

                  , gameResult
                  , gameResult'

                  , promotionPiece

                  , finger
                  , pendingOffers

                  , login
                  , loginTimeout
                  , password
                  , guestLogin
                  , unknownUsername
                  , loggedIn
                  , invalidPassword
                  , acknoledge
                  , settingsDone
                  ]

gameMove :: Parser FicsMessage
gameMove = GameMove <$> pure None <*> move

confirmGameMove :: Parser FicsMessage
confirmGameMove = do
  ctx <- commandHead 1 *> option None ("Illegal move" *> pure Illegal)
  move <- manyTill anyChar "<12>" *> moveOnly -- ^ there is potentially another pieceHolding in front
  ph <- option NullCommand (manyTill anyChar "<b1>" *> pieceHoldingOnly)
  return $ Boxed [GameMove ctx move, ph]

-- 11 = accept (match) ! don't delete this again: "You accept the match offer from"
acceptMatchOffer :: Parser FicsMessage
acceptMatchOffer = MatchAccepted <$> (commandHead 11 *> "You accept the match offer from" *> takeTill (== '<') *> move)

-- 155 = Seek (User selects a seek)
-- 158 = Play (Users' seek matches a seek already posted)
seek :: Parser FicsMessage
seek = Boxed <$> ((commandHead 155 <|> commandHead 158) *> sequence
  [ takeTill (== '<') *> SP.removeSeeks
  , takeTill (== '<') *> (MatchAccepted <$> move)])

gameCreation :: Parser FicsMessage
gameCreation = GameCreation <$> ("{Game " *> decimal <* takeTill (== ')') <* ") Creating ")

observe :: Parser FicsMessage
observe = Boxed <$> sequence [
    Observe <$> (commandHead 80 *> takeTill (=='<') *> move)
  , option NullCommand (takeTill (=='<') *> pieceHolding)]

noSuchGame :: Parser FicsMessage
noSuchGame = commandHead 80 *> "There is no such game." *> pure NoSuchGame

games :: Parser FicsMessage
games = Games <$> (commandHead 43 *> parseGamesList)

challenge :: Parser FicsMessage
challenge = MatchRequested <$> (Challenge
  <$> ("Challenge: " *> manyTill anyChar space)
  <*> ("(" *> skipSpace *> rating)
  <*> (") " *> manyTill anyChar space)
  <*> ("(" *> skipSpace *> rating)
  <*> (") " *> manyTill anyChar ".")) --unrated blitz 2 12."

matchDeclined :: Parser FicsMessage
matchDeclined = MatchDeclined <$> ("\"" *> manyTill anyChar "\" declines the match offer.")

matchOfferIdentical :: Parser FicsMessage
matchOfferIdentical = commandHead 73 *> "You are already offering an identical match to" *> pure MatchOfferIdentical

matchUserNotLoggedIn :: Parser FicsMessage
matchUserNotLoggedIn = MatchUserNotLoggedIn <$> (commandHead 73 *> manyTill anyChar " " <* "is not logged in.")

drawRequest :: Parser FicsMessage
drawRequest = manyTill anyChar space *> "offers you a draw." *> pure DrawRequest

drawRequestDeclined :: Parser FicsMessage
drawRequestDeclined = DrawRequestDeclined <$> manyTill anyChar space <* "declines the draw request."

abortRequest :: Parser FicsMessage
abortRequest = AbortRequest <$> (manyTill anyChar " " <* "would like to abort the game;")

abortRequestDeclined :: Parser FicsMessage
abortRequestDeclined = AbortRequestDeclined <$> manyTill anyChar space <* "declines the abort request."

takebackRequest :: Parser FicsMessage
takebackRequest = TakebackRequest
  <$> manyTill anyChar " " <* "would like to take back "
  <*> decimal <* " half move(s)."

takebackAccepted :: Parser FicsMessage
takebackAccepted = TakebackAccepted <$> manyTill anyChar " " <* "accepts the takeback request."

acceptTakeback :: Parser FicsMessage
acceptTakeback = GameMove <$>
  pure (Takeback Nothing) <*> -- ^ User accepted takeback himself
  (commandHead 11 *> "You accept the takeback request from" *> takeTill (== '<') *> move)

-- 10 = Abort
-- 11 = Accept (Draw, Abort)
-- 34 = Draw (Mutual)
-- 103 = Resign
gameResult :: Parser FicsMessage
gameResult = choice [commandHead 10, commandHead 11, commandHead 34, commandHead 103] *> gameResult'

gameResult' :: Parser FicsMessage
gameResult' = GameResult
  <$> (takeTill (== '{') *> "{Game " *> decimal)
  <*> (takeTill (== ')') *> ") " *> manyTill anyChar "} ")
  <*> ("1-0" *> pure WhiteWins <|> "0-1" *> pure BlackWins <|> "1/2-1/2" *> pure Draw <|> "*" *> pure Aborted)

promotionPiece :: Parser FicsMessage
promotionPiece = PromotionPiece <$> (commandHead 92 *> "Promotion piece set to " *>
  ("QUEEN" *> pure Queen <|> "BISHOP" *> pure Bishop <|> "KNIGHT" *> pure Knight <|> 	"ROOK" *> pure Rook <|> "KING" *> pure King))

finger :: Parser FicsMessage
finger = Finger
  <$> (commandHead 37 *> "Finger of " *> manyTill anyChar ":")
  <*> manyTill anyChar "\n\n\ETB"

pendingOffers :: Parser FicsMessage
pendingOffers = PendingOffers
  <$> (commandHead 87 *> (("There are no offers pending to other players.\n\n" *> pure []) <|>
                          ("Offers to other players:\n\n" *> sepBy pendingOffer "\n\n" <* "\n\nIf you wish to withdraw any of these offers type \"withdraw number\".\n\n")))
  <*> (("There are no offers pending from other players." *> pure []) <|>
        "Offers from other players:\n\n" *> sepBy pendingOffer "\n\n")

pendingOffer :: Parser PendingOffer
pendingOffer = PendingOffer <$> (skipSpace *> decimal <* ": ") <*> manyTill anyChar "."

login :: Parser FicsMessage
login = "login: " *> pure Login

loginTimeout :: Parser FicsMessage
loginTimeout = "**** LOGIN TIMEOUT ****" *> pure LoginTimeout

password :: Parser FicsMessage
password = "password: " *> pure Password

guestLogin :: Parser FicsMessage
guestLogin = GuestLogin <$> ("Press return to enter the server as \"" *> manyTill anyChar "\":")

unknownUsername :: Parser FicsMessage
unknownUsername = UnkownUsername <$> ("\"" *> manyTill anyChar "\" is not a registered name.")

-- | Beware the guest handles: ie GuestXWLW(U)
loggedIn :: Parser FicsMessage
loggedIn = LoggedIn
  <$> ("**** Starting FICS session as " *> (Prelude.head . splitOn "(") `fmap` manyTill anyChar " ****")

invalidPassword :: Parser FicsMessage
invalidPassword = "**** Invalid password! ****" *> pure InvalidPassword

acknoledge :: Parser FicsMessage
acknoledge = commandHead 519 *> char (chr 23) *> pure Acknoledge

settingsDone :: Parser FicsMessage
settingsDone = char (chr 23) *> pure SettingsDone
