{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.CommandMsgParser (
 parseFicsMessage
) where

import Macbeth.Fics.Api.Challenge
import Macbeth.Fics.FicsMessage hiding (move)
import Macbeth.Fics.Api.Game
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

                  , gameMove
                  , confirmGameMove
                  , accept
                  , playSeek
                  , observe
                  , pieceHolding

                  , matchRequested
                  , declinedChallenge

                  , games
                  , noSuchGame
                  , gameCreation

                  , abortRequest
                  , takebackRequest
                  , drawRequest

                  , gameResult
                  , gameResult'
                  , gameResultMutualDraw
                  , gameResultAcceptDrawOrAbort
                  , gameResultAbortGame
                  , gameResultAbortGame2

                  , seekInfoBlock
                  , seekMatchesAlreadyPosted

                  , finger
                  , pendingOffers
                  , offerAccepted
                  , offerDeclined
                  , matchUserNotLoggedIn
                  , identicalOffer

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

gameMove :: Parser FicsMessage
gameMove = GameMove <$> pure False <*> move

confirmGameMove :: Parser FicsMessage
confirmGameMove = do
  illegal <- commandHead 1 *> option False ("Illegal move" *> pure True)
  ph <- option NullCommand (takeTill (=='<') *> pieceHolding)
  move' <- takeTill (=='<') *> move
  return $ Boxed [ph, GameMove illegal move']

accept :: Parser FicsMessage
accept = MatchAccepted <$> (commandHead 11 *> takeTill (== '<') *> move)

playSeek :: Parser FicsMessage
playSeek = Boxed
  <$> sequence [ commandHead 158 *> "\n" *> SP.removeSeeks <* "\n"
               , takeTill (=='<') *> (MatchAccepted <$> move)]

seekMatchesAlreadyPosted :: Parser FicsMessage
seekMatchesAlreadyPosted = do
  commandHead 155
  option "" "You are unregistered - setting to unrated.\n"
  rs <- "Your seek matches one already posted by" *> takeTill (== '<') *> SP.removeSeeks <* "\n"
  mv <- takeTill (=='<') *> (MatchAccepted <$> move)
  return $ Boxed [rs, mv]

observe :: Parser FicsMessage
observe = Observe <$> (commandHead 80 *> takeTill (=='<') *> move)


games :: Parser FicsMessage
games = Games <$> (commandHead 43 *> parseGamesList)


noSuchGame :: Parser FicsMessage
noSuchGame = commandHead 80 *> "There is no such game." *> pure NoSuchGame


matchRequested :: Parser FicsMessage
matchRequested = MatchRequested <$> (Challenge
  <$> ("Challenge: " *> manyTill anyChar space)
  <*> ("(" *> skipSpace *> rating)
  <*> (") " *> manyTill anyChar space)
  <*> ("(" *> skipSpace *> rating)
  <*> (") " *> manyTill anyChar ".")) --unrated blitz 2 12."

declinedChallenge :: Parser FicsMessage
declinedChallenge = "\"" *> manyTill anyChar "\" declines the match offer." *> pure MatchDeclined

seekInfoBlock :: Parser FicsMessage
seekInfoBlock = Boxed
  <$> (commandHead 56 *> "seekinfo set.\n" *> sepBy (choice [ SP.clearSeek, SP.newSeek <* takeTill (== '\n')]) "\n")

gameCreation :: Parser FicsMessage
gameCreation = GameCreation
  <$> (skipSpace *> "{Game" *> space *> decimal)
  <*> (takeTill (== ')') *> ") Creating " *> manyTill anyChar "}")


drawRequest :: Parser FicsMessage
drawRequest = manyTill anyChar space *> "offers you a draw." *> pure DrawRequest

abortRequest :: Parser FicsMessage
abortRequest = AbortRequest <$> (manyTill anyChar " " <* "would like to abort the game;")

takebackRequest :: Parser FicsMessage
takebackRequest = TakebackRequest
  <$> manyTill anyChar " " <* "would like to take back "
  <*> decimal <* " half move(s)."


gameResult :: Parser FicsMessage
gameResult = commandHead 103 *> gameResult'

gameResultMutualDraw :: Parser FicsMessage
gameResultMutualDraw = commandHead 34 *> gameResult'

gameResultAcceptDrawOrAbort :: Parser FicsMessage
gameResultAcceptDrawOrAbort = commandHead 11 *> takeTill (== '{') *> (gameResult' <|> gameResultAbortGame)

gameResult' :: Parser FicsMessage
gameResult' = GameResult
  <$> (skipSpace *> "{Game" *> space *> decimal)
  <*> (takeTill (== ')') *> ") " *> manyTill anyChar "} ")
  <*> ("1-0" *> pure WhiteWins <|> "0-1" *> pure BlackWins <|>  "1/2-1/2" *> pure Draw)

gameResultAbortGame :: Parser FicsMessage
gameResultAbortGame = GameResult
  <$> ("{Game " *> decimal <* manyTill anyChar ") ")
  <*> manyTill anyChar "} *"
  <*> pure Aborted

gameResultAbortGame2 :: Parser FicsMessage
gameResultAbortGame2 = commandHead 10 *> choice ["\n", "The game has been aborted on move one.\n\n"] *> gameResultAbortGame


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

offerAccepted :: Parser FicsMessage
offerAccepted = manyTill anyChar " " *> "accepts the match offer." *> pure OfferAccepted

offerDeclined :: Parser FicsMessage
offerDeclined = manyTill anyChar " " *> "declines the match offer." *> pure OfferDeclined

identicalOffer :: Parser FicsMessage
identicalOffer = commandHead 73 *> "You are already offering an identical match to" *> pure IdenticalOffer

matchUserNotLoggedIn :: Parser FicsMessage
matchUserNotLoggedIn = MatchUserNotLoggedIn
  <$> (commandHead 73 *> manyTill anyChar " " <* "is not logged in.")

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

prompt :: Parser FicsMessage
prompt = "fics% " *> pure Prompt

acknoledge :: Parser FicsMessage
acknoledge = commandHead 519 *> char (chr 23) *> pure Acknoledge

settingsDone :: Parser FicsMessage
settingsDone = char (chr 23) *> pure SettingsDone
