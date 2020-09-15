module Macbeth.Fics.Parsers.MessageParser (
 parseMessage
) where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Message hiding (move)
import           Macbeth.Fics.Api.Move hiding (Observing)
import           Macbeth.Fics.Api.Game
import           Macbeth.Fics.Api.Offer
import           Macbeth.Fics.Api.Result
import           Macbeth.Fics.Parsers.MoveParser
import qualified Macbeth.Fics.Parsers.RatingParser as R
import qualified Macbeth.Fics.Parsers.Api as Api
import qualified Macbeth.Fics.Parsers.Chatting as Chatting
import qualified Macbeth.Fics.Parsers.GamesParser as GP
import qualified Macbeth.Fics.Parsers.Players as P
import qualified Macbeth.Fics.Parsers.SeekMsgParsers as SP
import qualified Macbeth.Fics.Parsers.Stored as Stored

parseMessage :: BS.ByteString -> Either String Message
parseMessage = parseOnly $ choice [
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

  , Stored.parser

  , promotionPiece

  , login
  , loginTimeout
  , password
  , guestLogin
  , unknownUsername
  , loggedIn
  , logOut
  , invalidPassword
  , abusiveBehavior
  , ping]


username :: Parser String
username = many1 letter_ascii


gameMove :: Parser Message
gameMove = GameMove <$> pure None <*> move


illegalMove :: Parser Message
illegalMove = IllegalMove <$> ("Illegal move (" *> fmap BS.unpack (takeTill (== ')')) <* ").")


newGameParamsUser :: Parser Message
newGameParamsUser = NewGameParamsUser <$> ("Creating: " *> gameParams' True)


newGameIdUser :: Parser Message
newGameIdUser = NewGameIdUser
  <$> ("{Game " *> Api.gameId)
  <* " (" <* username
  <* (" vs. " <* username <* ") Creating ")


observing :: Parser Message
observing = Observing <$> ("Game " *> Api.gameId <* ": ") <*> gameParams' False


noSuchGame :: Parser Message
noSuchGame = "There is no such game." *> pure NoSuchGame


userNotLoggedIn :: Parser Message
userNotLoggedIn = UserNotLoggedIn <$> username <* " is not logged in."


challenge :: Parser Message
challenge = MatchRequested <$> (Challenge <$> ("Challenge: " *> gameParams' True))


drawRequest :: Parser Message
drawRequest = DrawRequest <$> username <* " offers you a draw."


abortRequest :: Parser Message
abortRequest = AbortRequest <$> username <* " would like to abort the game;"


takebackRequest :: Parser Message
takebackRequest = TakebackRequest <$> username <* " would like to take back " <*> decimal <* " half move(s)."


takebackAccepted :: Parser Message
takebackAccepted = (TakebackAccepted . Just <$> username) <* " accepts the takeback request."


takebackAccepted' :: Parser Message
takebackAccepted' = TakebackAccepted <$> pure Nothing <* "You accept the takeback request"


oponentDecline :: Parser Message
oponentDecline = OponentDecline
  <$> (username <* " declines the ")
  <*> ("draw" *> pure DrawReq <|> "takeback" *> pure TakebackReq <|> "abort" *> pure AbortReq <|> "match" *> pure MatchReq)


gameResult' :: Parser Message
gameResult' = GameResult <$> (Result
  <$> (takeTill (== '{') *> "{Game " *> Api.gameId)
  <*> (takeTill (== '(') *> "(" *> manyTill anyChar " vs. ")
  <*> manyTill anyChar ") "
  <*> manyTill anyChar "} "
  <*> ("1-0" *> pure WhiteWins <|> "0-1" *> pure BlackWins <|> "1/2-1/2" *> pure Draw <|> "*" *> pure Aborted))


promotionPiece :: Parser Message
promotionPiece = PromotionPiece <$> ("Promotion piece set to " *> ("QUEEN" *> pure Queen <|>
  "BISHOP" *> pure Bishop <|> "KNIGHT" *> pure Knight <|> "ROOK" *> pure Rook <|> "KING" *> pure King))


pending :: Parser Message
pending = Pending <$> (PendingOffer
  <$> (("<pf>" *> pure From) <|> ("<pt>" *> pure To))
  <*> (" " *> decimal)
  <*> (" w=" *> P.userHandle)
  <*> (" t=" *> manyTill anyChar " ")
  <*> ("p=" *> many1 anyChar))


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


pendingRemoved :: Parser Message
pendingRemoved = PendingRemoved <$> ("<pr> " *> decimal)


login :: Parser Message
login = "login: " *> pure LoginPrompt


loginTimeout :: Parser Message
loginTimeout = "**** LOGIN TIMEOUT ****" *> pure LoginTimeout


password :: Parser Message
password = "password: " *> pure Password


guestLogin :: Parser Message
guestLogin = GuestLogin <$> ("Press return to enter the server as \"" *> username <* "\":")


unknownUsername :: Parser Message
unknownUsername = UnkownUsername <$> ("\"" *> username <* "\" is not a registered name.")


loggedIn :: Parser Message
loggedIn = LoggedIn <$> ("**** Starting FICS session as " *> P.userHandle <* " ****")


logOut :: Parser Message
logOut = "Logging you out." *> pure LogOut

abusiveBehavior :: Parser Message
abusiveBehavior = "Due to abusive behavior, no registered users may use this server from your site." *> pure AbusiveBehavior

invalidPassword :: Parser Message
invalidPassword = "**** Invalid password! ****" *> pure InvalidPassword


ping :: Parser Message
ping = Ping
  <$> (":min/avg/max/mdev = " *> round `fmap` double)
  <*> ("/" *> round `fmap` double)
  <*> ("/" *> round `fmap` double)
