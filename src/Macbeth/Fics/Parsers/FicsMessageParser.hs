{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.FicsMessageParser (
 parseFicsMessage
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Challenge
import Macbeth.Fics.FicsMessage hiding (move)
import Macbeth.Fics.Api.Move hiding (Observing)
import Macbeth.Fics.Api.PendingOffer
import Macbeth.Fics.Api.Result
import Macbeth.Fics.Parsers.GamesParser
import Macbeth.Fics.Parsers.MoveParser
import Macbeth.Fics.Parsers.RatingParser
import qualified Macbeth.Fics.Parsers.Api as Api
import qualified Macbeth.Fics.Parsers.Chatting as Chatting
import qualified Macbeth.Fics.Parsers.Players as P
import qualified Macbeth.Fics.Parsers.SeekMsgParsers as SP

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS


parseFicsMessage :: BS.ByteString -> Either String FicsMessage
parseFicsMessage = parseOnly parser where
  parser = choice [ SP.clearSeek
                  , SP.newSeek
                  , SP.removeSeeks
                  , SP.seekNotAvailable

                  , gameMove
                  , gameCreation
                  , pieceHolding

                  , challenge
                  , matchDeclined
                  , matchUserNotLoggedIn

                  , games
                  , observing
                  , noSuchGame
                  , userNotLoggedIn

                  , abortRequest
                  , abortRequestDeclined

                  , takebackRequest
                  , takebackAccepted
                  , acceptTakeback
                  , takebackRequestDeclined

                  , drawRequest
                  , drawRequestDeclined

                  , gameResult'

                  , promotionPiece

                  , P.finger
                  , P.history
                  , P.players
                  , P.partnerNotOpen
                  , P.partnerOffer
                  , P.partnerAccepted
                  , P.partnerDeclined

                  , Chatting.says
                  , Chatting.tell
                  , Chatting.told

                  , pending
                  , pendingTo
                  , pendingRemoved
                  , pendingWithdraw

                  , login
                  , loginTimeout
                  , password
                  , guestLogin
                  , unknownUsername
                  , loggedIn
                  , invalidPassword
                  , ping
                  ]

gameMove :: Parser FicsMessage
gameMove = GameMove <$> pure None <*> move

gameCreation :: Parser FicsMessage
gameCreation = GameCreation <$> ("{Game " *> Api.gameId <* takeTill (== ')') <* ") Creating ")

observing :: Parser FicsMessage
observing = Observing <$> ("You are now observing game " *> Api.gameId)

noSuchGame :: Parser FicsMessage
noSuchGame = "There is no such game." *> pure NoSuchGame

userNotLoggedIn :: Parser FicsMessage
userNotLoggedIn = UserNotLoggedIn <$> (Api.commandHead 80 *> many1 letter_ascii <* " is not logged in.\n\ETB")

games :: Parser FicsMessage
games = Games <$> (Api.commandHead 43 *> parseGamesList)

challenge :: Parser FicsMessage
challenge = MatchRequested <$> (Challenge
  <$> ("Challenge: " *> manyTill anyChar space)
  <*> ("(" *> skipSpace *> rating)
  <*> (") " *> manyTill anyChar space)
  <*> ("(" *> skipSpace *> rating)
  <*> (") " *> manyTill anyChar ".")) --unrated blitz 2 12."

matchDeclined :: Parser FicsMessage
matchDeclined = MatchDeclined <$> ("\"" *> manyTill anyChar "\" declines the match offer.")

matchUserNotLoggedIn :: Parser FicsMessage
matchUserNotLoggedIn = MatchUserNotLoggedIn <$> (Api.commandHead 73 *> manyTill anyChar " " <* "is not logged in.")

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

takebackRequestDeclined :: Parser FicsMessage
takebackRequestDeclined = TakebackRequestDeclined <$> manyTill anyChar space <* "declines the takeback request."

acceptTakeback :: Parser FicsMessage
acceptTakeback = GameMove <$>
  pure (Takeback Nothing) <*> -- ^ User accepted takeback himself
  (Api.commandHead 11 *> "You accept the takeback request from" *> takeTill (== '<') *> move)

gameResult' :: Parser FicsMessage
gameResult' = GameResult <$> (Result
  <$> (takeTill (== '{') *> "{Game " *> Api.gameId)
  <*> (takeTill (== '(') *> "(" *> manyTill anyChar " vs. ")
  <*> manyTill anyChar ") "
  <*> manyTill anyChar "} "
  <*> ("1-0" *> pure WhiteWins <|> "0-1" *> pure BlackWins <|> "1/2-1/2" *> pure Draw <|> "*" *> pure Aborted))

promotionPiece :: Parser FicsMessage
promotionPiece = PromotionPiece <$> (Api.commandHead 92 *> "Promotion piece set to " *>
  ("QUEEN" *> pure Queen <|> "BISHOP" *> pure Bishop <|> "KNIGHT" *> pure Knight <|> "ROOK" *> pure Rook <|> "KING" *> pure King))

pending :: Parser FicsMessage
pending = Pending <$> (PendingOffer
  <$> (("<pf>" *> pure From) <|> ("<pt>" *> pure To))
  <*> (" " *> decimal)
  <*> (" w=" *> P.userHandle)
  <*> (" t=" *> manyTill anyChar " ")
  <*> ("p=" *> manyTill anyChar "\n"))

-- BLK_MATCH 73
pendingTo :: Parser FicsMessage
pendingTo = Api.commandHead 73 *> takeTill (=='<') *> pending

pendingRemoved :: Parser FicsMessage
pendingRemoved = PendingRemoved <$> ("<pr> " *> decimal)

pendingWithdraw :: Parser FicsMessage
pendingWithdraw = Api.commandHead 147 *> takeTill (=='<') *> pendingRemoved

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

loggedIn :: Parser FicsMessage
loggedIn = LoggedIn <$> ("**** Starting FICS session as " *> P.userHandle <* " ****")

invalidPassword :: Parser FicsMessage
invalidPassword = "**** Invalid password! ****" *> pure InvalidPassword

ping :: Parser FicsMessage
ping = Ping
  <$> (":min/avg/max/mdev = " *> round `fmap` double)
  <*> ("/" *> round `fmap` double)
  <*> ("/" *> round `fmap` double)
