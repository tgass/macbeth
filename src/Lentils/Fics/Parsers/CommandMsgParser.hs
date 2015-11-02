{-# LANGUAGE OverloadedStrings #-}

module Lentils.Fics.Parsers.CommandMsgParser (
 parseCommandMsg
) where

import Lentils.Api.Challenge
import Lentils.Api.CommandMsg
import Lentils.Api.Game
import Lentils.Fics.Parsers.GamesParser
import Lentils.Fics.Parsers.MoveParser2
import Lentils.Fics.Parsers.RatingParser
import qualified Lentils.Fics.Parsers.SeekMsgParsers as SP

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

                  , matchRequested
                  , matchUpdated
                  , declinedChallenge
                  , drawOffered
                  , drawDeclined

                  , games
                  , playSeek
                  , observe
                  , noSuchGame
                  , accept
                  , gameResult
                  , gameResult'
                  , gameResultMutualDraw
                  , gameResultAcceptDraw

                  , confirmGameMove
                  , seekInfoBlock
                  , seekMatchesAlreadyPosted
                  , gameMove

                  , login
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

matchUpdated :: Parser CommandMsg
matchUpdated = MatchUpdated <$> manyTill anyChar space <* "updates the match request."

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

login :: Parser CommandMsg
login = "login: " *> pure Login

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
