{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.CommandMsgParser (
 parseCommandMsg
) where

import Macbeth.Api.Api
import Macbeth.Api.Challenge
import Macbeth.Api.CommandMsg
import Macbeth.Api.Game
import Macbeth.Fics.Parsers.GamesParser
import Macbeth.Fics.Parsers.MoveParser2
import Macbeth.Fics.Parsers.RatingParser
import qualified Macbeth.Fics.Parsers.SeekMsgParsers as SP

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Char
import Data.List.Split (splitOn)
import qualified Data.ByteString.Char8 as BS



parseCommandMsg :: BS.ByteString -> Either String CommandMsg
parseCommandMsg = parseOnly parser where
  parser = choice [ SP.clearSeek
                  , SP.newSeek
                  , SP.removeSeeks

                  , gameMove
                  , confirmGameMove

                  , matchRequested
                  , declinedChallenge
                  , drawOffered
                  , drawDeclined

                  , games
                  , playSeek
                  , observe
                  , removingObservedGame
                  , removingObservedGame2
                  , noSuchGame
                  , accept
                  , gameCreation
                  , gameResult
                  , gameResult'
                  , gameResultMutualDraw
                  , gameResultAcceptDraw

                  , seekInfoBlock
                  , seekMatchesAlreadyPosted

                  , finger
                  , pendingOffers
                  , offerAccepted
                  , offerDeclined
                  , matchUserNotLoggedIn
                  , identicalOffer

                  , abortRequested
                  , abortRequest
                  , abortDeclined
                  , abortAccepted
                  , abortedGame
                  , abortedGame2

                  , login
                  , loginTimeout
                  , password
                  , guestLogin
                  , unknownUsername
                  , loggedIn
                  , invalidPassword
                  , prompt
                  , acknoledge
                  , settingsDone
                  ]

games :: Parser CommandMsg
games = Games <$> (commandHead 43 *> parseGamesList)

observe :: Parser CommandMsg
observe = Observe <$> (commandHead 80 *> move)

removingObservedGame :: Parser CommandMsg
removingObservedGame = "Removing game " *> decimal *> " from observation list." *> pure RemovingObservedGame

removingObservedGame2 :: Parser CommandMsg
removingObservedGame2 = commandHead 138 *> removingObservedGame

noSuchGame :: Parser CommandMsg
noSuchGame = commandHead 80 *> "There is no such game." *> pure NoSuchGame

confirmGameMove :: Parser CommandMsg
confirmGameMove = GameMove <$> (commandHead 1 *> move)

gameMove :: Parser CommandMsg
gameMove = GameMove <$> move

playSeek :: Parser CommandMsg
playSeek = do
  commandHead 158
  rs <- "\n" *> SP.removeSeeks <* "\n"
  mv <- takeTill (=='<') *> (GameMove <$> move)
  return $ Boxed [rs, mv]

matchRequested :: Parser CommandMsg
matchRequested = MatchRequested <$> (Challenge
  <$> ("Challenge: " *> manyTill anyChar space)
  <*> ("(" *> rating)
  <*> (") " *> manyTill anyChar space)
  <*> ("(" *> rating)
  <*> (") " *> manyTill anyChar ".")) --unrated blitz 2 12."

accept :: Parser CommandMsg
accept = MatchAccepted <$> (commandHead 11 *> move)

declinedChallenge :: Parser CommandMsg
declinedChallenge = "\"" *> manyTill anyChar "\" declines the match offer." *> pure MatchDeclined

seekInfoBlock :: Parser CommandMsg
seekInfoBlock = Boxed
  <$> (commandHead 56 *> "seekinfo set.\n" *> sepBy (choice [ SP.clearSeek, SP.newSeek <* takeTill (== '\n')]) "\n")

seekMatchesAlreadyPosted :: Parser CommandMsg
seekMatchesAlreadyPosted = do
  commandHead 155
  option "" "You are unregistered - setting to unrated.\n"
  rs <- "Your seek matches one already posted by" *> takeTill (== '<') *> SP.removeSeeks <* "\n"
  mv <- takeTill (=='<') *> (GameMove <$> move)
  return $ Boxed [rs, mv]

drawOffered :: Parser CommandMsg
drawOffered = manyTill anyChar space *> "offers you a draw." *> pure DrawOffered

drawDeclined :: Parser CommandMsg
drawDeclined = manyTill anyChar space *> "declines the draw request." *> pure DrawDeclined

gameCreation :: Parser CommandMsg
gameCreation = GameCreation
  <$> (skipSpace *> "{Game" *> space *> decimal)
  <*> (takeTill (== ')') *> ") Creating " *> manyTill anyChar "}")

gameResult :: Parser CommandMsg
gameResult = commandHead 103 *> gameResult'

gameResultMutualDraw :: Parser CommandMsg
gameResultMutualDraw = commandHead 34 *> gameResult'

gameResultAcceptDraw :: Parser CommandMsg
gameResultAcceptDraw = commandHead 11 *> takeTill (== '{') *> gameResult'

gameResult' :: Parser CommandMsg
gameResult' = GameResult
  <$> (skipSpace *> "{Game" *> space *> decimal)
  <*> (takeTill (== ')') *> ") " *> manyTill anyChar "} ")
  <*> ("1-0" *> pure WhiteWins <|> "0-1" *> pure BlackWins <|>  "1/2-1/2" *> pure Draw)

finger :: Parser CommandMsg
finger = Finger
  <$> (commandHead 37 *> "Finger of " *> manyTill anyChar ":")
  <*> manyTill anyChar "\n\n\ETB"

pendingOffers :: Parser CommandMsg
pendingOffers = PendingOffers
  <$> (commandHead 87 *> (("There are no offers pending to other players.\n\n" *> pure []) <|>
                          ("Offers to other players:\n\n" *> sepBy pendingOffer "\n\n" <* "\n\nIf you wish to withdraw any of these offers type \"withdraw number\".\n\n")))
  <*> (("There are no offers pending from other players." *> pure []) <|>
        "Offers from other players:\n\n" *> sepBy pendingOffer "\n\n")

pendingOffer :: Parser PendingOffer
pendingOffer = PendingOffer <$> (skipSpace *> decimal <* ": ") <*> manyTill anyChar "."

offerAccepted :: Parser CommandMsg
offerAccepted = manyTill anyChar " " *> "accepts the match offer." *> pure OfferAccepted

offerDeclined :: Parser CommandMsg
offerDeclined = manyTill anyChar " " *> "declines the match offer." *> pure OfferDeclined

identicalOffer :: Parser CommandMsg
identicalOffer = commandHead 73 *> "You are already offering an identical match to" *> pure IdenticalOffer

matchUserNotLoggedIn :: Parser CommandMsg
matchUserNotLoggedIn = MatchUserNotLoggedIn
  <$> (commandHead 73 *> manyTill anyChar " " <* "is not logged in.")

abortRequest :: Parser CommandMsg
abortRequest = commandHead 10 *> "Abort request sent." *> pure AbortRequest

abortRequested :: Parser CommandMsg
abortRequested = AbortRequested
  <$> (manyTill anyChar " " <* "would like to abort the game;")

abortDeclined :: Parser CommandMsg
abortDeclined = ((commandHead 33 *> "You decline the abort request from") <|>
  (manyTill anyChar " " *> "declines the abort request.")) *> pure AbortDeclined

abortAccepted :: Parser CommandMsg
abortAccepted = ((commandHead 11 *> "You accept the abort request from") <|>
  (manyTill anyChar " " *> "accepts the abort request.")) *> pure AbortAccepted

abortedGame :: Parser CommandMsg
abortedGame = GameResult
  <$> ("{Game " *> decimal <* manyTill anyChar ") ")
  <*> manyTill anyChar "} *"
  <*> pure Aborted

abortedGame2 :: Parser CommandMsg
abortedGame2 = commandHead 10 *> "The game has been aborted on move one.\n\n" *> abortedGame

login :: Parser CommandMsg
login = "login: " *> pure Login

loginTimeout :: Parser CommandMsg
loginTimeout = "**** LOGIN TIMEOUT ****" *> pure LoginTimeout

password :: Parser CommandMsg
password = "password: " *> pure Password

guestLogin :: Parser CommandMsg
guestLogin = GuestLogin <$> ("Press return to enter the server as \"" *> manyTill anyChar "\":")

unknownUsername :: Parser CommandMsg
unknownUsername = UnkownUsername <$> ("\"" *> manyTill anyChar "\" is not a registered name.")

-- | Beware the guest handles: ie GuestXWLW(U)
loggedIn :: Parser CommandMsg
loggedIn = LoggedIn
  <$> ("**** Starting FICS session as " *> (Prelude.head . splitOn "(") `fmap` manyTill anyChar " ****")

invalidPassword :: Parser CommandMsg
invalidPassword = "**** Invalid password! ****" *> pure InvalidPassword

prompt :: Parser CommandMsg
prompt = "fics% " *> pure Prompt

acknoledge :: Parser CommandMsg
acknoledge = commandHead 519 *> char (chr 23) *> pure Acknoledge

settingsDone :: Parser CommandMsg
settingsDone = char (chr 23) *> pure SettingsDone


{- HELPER -}
data CommandHead = CommandHead Int deriving (Show)

commandHead :: Int -> Parser CommandHead
commandHead code = do
  char $ chr 21
  id <- decimal
  char $ chr 22
  string $ BS.pack $ show code
  char $ chr 22
  return $ CommandHead id
