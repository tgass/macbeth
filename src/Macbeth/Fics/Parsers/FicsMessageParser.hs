{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.FicsMessageParser (
 parseFicsMessage
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.FicsMessage hiding (move)
import Macbeth.Fics.Api.Move hiding (Observing)
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Result
import Macbeth.Fics.Parsers.MoveParser
import qualified Macbeth.Fics.Parsers.RatingParser as R
import qualified Macbeth.Fics.Parsers.Api as Api
import qualified Macbeth.Fics.Parsers.Chatting as Chatting
import qualified Macbeth.Fics.Parsers.GamesParser as GP
import qualified Macbeth.Fics.Parsers.Players as P
import qualified Macbeth.Fics.Parsers.SeekMsgParsers as SP

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS


parseFicsMessage :: BS.ByteString -> Either String FicsMessage
parseFicsMessage = parseOnly $ choice [
    SP.clearSeek
  , SP.newSeek
  , SP.removeSeeks
  , SP.seekNotAvailable

  , GP.gamesList

  , gameMove
  , illegalMove
  , gameResult'
  , pieceHolding

  , challenge
  , newGameParamsUser
  , newGameIdUser

  , observing
  , noSuchGame
  , userNotLoggedIn

  , abortRequest
  , drawRequest
  , takebackRequest

  , takebackAccepted
  , takebackAccepted'
  , oponentDecline

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
  , pendingRemoved

  , promotionPiece

  , login
  , loginTimeout
  , password
  , guestLogin
  , unknownUsername
  , loggedIn
  , invalidPassword
  , ping]


username :: Parser String
username = many1 letter_ascii


gameMove :: Parser FicsMessage
gameMove = GameMove <$> pure None <*> move


illegalMove :: Parser FicsMessage
illegalMove = IllegalMove <$> ("Illegal move (" *> fmap BS.unpack (takeTill (== ')')) <* ").")


newGameParamsUser :: Parser FicsMessage
newGameParamsUser = NewGameParamsUser <$> ("Creating: " *> gameParams' True)


newGameIdUser :: Parser FicsMessage
newGameIdUser = NewGameIdUser
  <$> ("{Game " *> Api.gameId)
  <* " (" <* username
  <* (" vs. " <* username <* ") Creating ")


observing :: Parser FicsMessage
observing = Observing <$> ("Game " *> Api.gameId <* ": ") <*> gameParams' False


noSuchGame :: Parser FicsMessage
noSuchGame = "There is no such game." *> pure NoSuchGame


userNotLoggedIn :: Parser FicsMessage
userNotLoggedIn = UserNotLoggedIn <$> username <* " is not logged in."


challenge :: Parser FicsMessage
challenge = MatchRequested <$> (Challenge <$> ("Challenge: " *> gameParams' True))


drawRequest :: Parser FicsMessage
drawRequest = DrawRequest <$> username <* " offers you a draw."


abortRequest :: Parser FicsMessage
abortRequest = AbortRequest <$> username <* " would like to abort the game;"


takebackRequest :: Parser FicsMessage
takebackRequest = TakebackRequest <$> username <* " would like to take back " <*> decimal <* " half move(s)."


takebackAccepted :: Parser FicsMessage
takebackAccepted = (TakebackAccepted . Just <$> username) <* " accepts the takeback request."


takebackAccepted' :: Parser FicsMessage
takebackAccepted' = TakebackAccepted <$> pure Nothing <* "You accept the takeback request"


oponentDecline :: Parser FicsMessage
oponentDecline = OponentDecline
  <$> (username <* " declines the ")
  <*> ("draw" *> pure DrawReq <|> "takeback" *> pure TakebackReq <|> "abort" *> pure AbortReq <|> "match" *> pure MatchReq)


gameResult' :: Parser FicsMessage
gameResult' = GameResult <$> (Result
  <$> (takeTill (== '{') *> "{Game " *> Api.gameId)
  <*> (takeTill (== '(') *> "(" *> manyTill anyChar " vs. ")
  <*> manyTill anyChar ") "
  <*> manyTill anyChar "} "
  <*> ("1-0" *> pure WhiteWins <|> "0-1" *> pure BlackWins <|> "1/2-1/2" *> pure Draw <|> "*" *> pure Aborted))


promotionPiece :: Parser FicsMessage
promotionPiece = PromotionPiece <$> ("Promotion piece set to " *> ("QUEEN" *> pure Queen <|>
  "BISHOP" *> pure Bishop <|> "KNIGHT" *> pure Knight <|> "ROOK" *> pure Rook <|> "KING" *> pure King))


pending :: Parser FicsMessage
pending = Pending <$> (PendingOffer
  <$> (("<pf>" *> pure From) <|> ("<pt>" *> pure To))
  <*> (" " *> decimal)
  <*> (" w=" *> P.userHandle)
  <*> (" t=" *> manyTill anyChar " ")
  <*> ("p=" *> ((MatchDetails <$> gameParams' True) <|> ("#" *> pure DrawOffer))))


gameParams' :: Bool -> Parser GameParams
gameParams' isGameUser = GameParams
  <$> pure isGameUser
  <*> username
  <*> (" (" *> skipSpace *> R.rating)
  <*> (") " *> username)
  <*> (" (" *> skipSpace *> R.rating <* ") ")
  <*> (("rated" *> pure True) <|> ("unrated" *> pure False))
  <*> (skipSpace *> manyTill anyChar " ")
  <*> (skipSpace *> decimal)
  <*> (skipSpace *> decimal)


pendingRemoved :: Parser FicsMessage
pendingRemoved = PendingRemoved <$> ("<pr> " *> decimal)


login :: Parser FicsMessage
login = "login: " *> pure LoginPrompt


loginTimeout :: Parser FicsMessage
loginTimeout = "**** LOGIN TIMEOUT ****" *> pure LoginTimeout


password :: Parser FicsMessage
password = "password: " *> pure Password


guestLogin :: Parser FicsMessage
guestLogin = GuestLogin <$> ("Press return to enter the server as \"" *> username <* "\":")


unknownUsername :: Parser FicsMessage
unknownUsername = UnkownUsername <$> ("\"" *> username <* "\" is not a registered name.")


loggedIn :: Parser FicsMessage
loggedIn = LoggedIn <$> ("**** Starting FICS session as " *> P.userHandle <* " ****")


invalidPassword :: Parser FicsMessage
invalidPassword = "**** Invalid password! ****" *> pure InvalidPassword


ping :: Parser FicsMessage
ping = Ping
  <$> (":min/avg/max/mdev = " *> round `fmap` double)
  <*> ("/" *> round `fmap` double)
  <*> ("/" *> round `fmap` double)
