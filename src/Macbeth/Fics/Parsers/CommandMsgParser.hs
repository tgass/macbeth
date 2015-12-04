{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.CommandMsgParser (
 parseCommandMsg
) where

import Macbeth.Api.Api
import Macbeth.Api.Challenge
import Macbeth.Api.CommandMsg
import Macbeth.Api.Game
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



parseCommandMsg :: BS.ByteString -> Either String CommandMsg
parseCommandMsg = parseOnly parser where
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
                  , removingObservedGame
                  , removingObservedGame2
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

gameMove :: Parser CommandMsg
gameMove = GameMove <$> move

confirmGameMove :: Parser CommandMsg
confirmGameMove = Boxed <$> sequence [ commandHead 1 *> option NullCommand (takeTill (=='<') *> pieceHolding)
                                     , GameMove <$> (takeTill (=='<') *> move)
                                     ]

accept :: Parser CommandMsg
accept = MatchAccepted <$> (commandHead 11 *> takeTill (== '<') *> move)

playSeek :: Parser CommandMsg
playSeek = Boxed
  <$> sequence [ commandHead 158 *> "\n" *> SP.removeSeeks <* "\n"
               , takeTill (=='<') *> (MatchAccepted <$> move)]

seekMatchesAlreadyPosted :: Parser CommandMsg
seekMatchesAlreadyPosted = do
  commandHead 155
  option "" "You are unregistered - setting to unrated.\n"
  rs <- "Your seek matches one already posted by" *> takeTill (== '<') *> SP.removeSeeks <* "\n"
  mv <- takeTill (=='<') *> (MatchAccepted <$> move)
  return $ Boxed [rs, mv]

observe :: Parser CommandMsg
observe = Observe <$> (commandHead 80 *> takeTill (=='<') *> move)




games :: Parser CommandMsg
games = Games <$> (commandHead 43 *> parseGamesList)

removingObservedGame :: Parser CommandMsg
removingObservedGame = "Removing game " *> decimal *> " from observation list." *> pure RemovingObservedGame

removingObservedGame2 :: Parser CommandMsg
removingObservedGame2 = commandHead 138 *> removingObservedGame

noSuchGame :: Parser CommandMsg
noSuchGame = commandHead 80 *> "There is no such game." *> pure NoSuchGame


matchRequested :: Parser CommandMsg
matchRequested = MatchRequested <$> (Challenge
  <$> ("Challenge: " *> manyTill anyChar space)
  <*> ("(" *> rating)
  <*> (") " *> manyTill anyChar space)
  <*> ("(" *> rating)
  <*> (") " *> manyTill anyChar ".")) --unrated blitz 2 12."

declinedChallenge :: Parser CommandMsg
declinedChallenge = "\"" *> manyTill anyChar "\" declines the match offer." *> pure MatchDeclined

seekInfoBlock :: Parser CommandMsg
seekInfoBlock = Boxed
  <$> (commandHead 56 *> "seekinfo set.\n" *> sepBy (choice [ SP.clearSeek, SP.newSeek <* takeTill (== '\n')]) "\n")

gameCreation :: Parser CommandMsg
gameCreation = GameCreation
  <$> (skipSpace *> "{Game" *> space *> decimal)
  <*> (takeTill (== ')') *> ") Creating " *> manyTill anyChar "}")


drawRequest :: Parser CommandMsg
drawRequest = manyTill anyChar space *> "offers you a draw." *> pure DrawRequest

abortRequest :: Parser CommandMsg
abortRequest = AbortRequest <$> (manyTill anyChar " " <* "would like to abort the game;")

takebackRequest :: Parser CommandMsg
takebackRequest = TakebackRequest
  <$> manyTill anyChar " " <* "would like to take back "
  <*> decimal <* " half move(s)."


gameResult :: Parser CommandMsg
gameResult = commandHead 103 *> gameResult'

gameResultMutualDraw :: Parser CommandMsg
gameResultMutualDraw = commandHead 34 *> gameResult'

gameResultAcceptDrawOrAbort :: Parser CommandMsg
gameResultAcceptDrawOrAbort = commandHead 11 *> takeTill (== '{') *> (gameResult' <|> gameResultAbortGame)

gameResult' :: Parser CommandMsg
gameResult' = GameResult
  <$> (skipSpace *> "{Game" *> space *> decimal)
  <*> (takeTill (== ')') *> ") " *> manyTill anyChar "} ")
  <*> ("1-0" *> pure WhiteWins <|> "0-1" *> pure BlackWins <|>  "1/2-1/2" *> pure Draw)

gameResultAbortGame :: Parser CommandMsg
gameResultAbortGame = GameResult
  <$> ("{Game " *> decimal <* manyTill anyChar ") ")
  <*> manyTill anyChar "} *"
  <*> pure Aborted

gameResultAbortGame2 :: Parser CommandMsg
gameResultAbortGame2 = commandHead 10 *> choice ["\n", "The game has been aborted on move one.\n\n"] *> gameResultAbortGame


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
