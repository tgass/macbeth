{-# LANGUAGE OverloadedStrings #-}

module CommandMsgParser (
 parseCommandMsg
) where

import Api
import CommandMsg
import GamesParser
import Move
import MoveParser2
import qualified ParseUtils as Utils
import qualified SeekMsgParsers as SP

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Char
import Data.List.Split (splitOn)
import qualified Data.ByteString.Char8 as BS



parseCommandMsg :: BS.ByteString -> Either String CommandMsg
parseCommandMsg str = parseOnly parser str where
  parser = choice [ SP.clearSeek
                  , SP.newSeek
                  , SP.removeSeeks

                  , challenge
                  , declinedChallenge
                  , drawOffered

                  , games
                  , observe
                  , accept
                  , gameResult
                  , gameResult'
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
games = Games <$> (commandHead 43 *> paresGamesList)

observe :: Parser CommandMsg
observe = Observe <$> (commandHead 80 *> move)


{- *** Moves *** -}
confirmGameMove :: Parser CommandMsg
confirmGameMove = GameMove <$> (commandHead 1 *> move)

gameMove :: Parser CommandMsg
gameMove = GameMove <$> move


{- *** Match/Challenge ***-}
challenge :: Parser CommandMsg
challenge = Challenge
  <$> ("Challenge: " *> manyTill anyChar space)
  <*> ("(" *> Utils.rating)
  <*> (") " *> manyTill anyChar space)
  <*> ("(" *> Utils.rating)
  <*> (") " *> manyTill anyChar ".") --unrated blitz 2 12."

accept :: Parser CommandMsg
accept = AcceptChallenge <$> (commandHead 11 *> move)

declinedChallenge :: Parser CommandMsg
declinedChallenge = "\"" *> manyTill anyChar "\" declines the match offer." *> pure DeclineChallenge

--TODO Adjourns challenge


seekInfoBlock :: Parser CommandMsg
seekInfoBlock = Boxed
  <$> (commandHead 56 *> "seekinfo set.\n" *> sepBy (choice [ SP.clearSeek, SP.newSeek <* takeTill (== '\n')]) "\n")

seekMatchesAlreadyPosted :: Parser CommandMsg
seekMatchesAlreadyPosted = do
  commandHead 115
  option "" "You are unregistered - setting to unrated.\n"
  rs <- "Your seek matches one already posted by" *> takeTill (== '<') *> SP.removeSeeks
  mv <- takeTill (=='<') *> (GameMove <$> move)
  return $ Boxed [rs, mv]

drawOffered :: Parser CommandMsg
drawOffered = manyTill anyChar space *> "offers you a draw." *> pure DrawOffered

gameResult :: Parser CommandMsg
gameResult = commandHead 103 *> gameResult'

gameResult' :: Parser CommandMsg
gameResult' = GameResult
  <$> (skipSpace *> "{Game" *> space *> decimal)
  <*> (takeTill (== ')') *> ") " *> manyTill anyChar "} ")
  <*> ("1-0" *> pure WhiteWins <|> "0-1" *> pure BlackWins <|>  "1/2-1/2" *> pure Draw)


{- *** LOGIN *** -}
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
acknoledge = commandHead 519 *> (char $ chr 23) *> pure Acknoledge

settingsDone :: Parser CommandMsg
settingsDone = (char $ chr 23) *> pure SettingsDone


{- HELPER -}

commandHead :: Int -> Parser CommandHead
commandHead code = do
  char $ chr 21
  id <- decimal
  char $ chr 22
  string $ BS.pack $ show code
  char $ chr 22
  return $ CommandHead id


{- TEST DATA -}

creatingGame' = BS.pack "Creating: Altivolous (1086) Schoon (1013) rated blitz 5 3"
seekMatchesAlreadyPosted' = BS.pack "Your seek matches one already posted by GuestJYQC.\n\n<sr> 119\nfics% \nCreating: GuestJYQC (++++) GuestNGCB (++++) unrated blitz 2 12\n{Game 364 (GuestJYQC vs. GuestNGCB) Creating unrated blitz match.}\n\a\n<12> rnbqkbnr pppppppp -------- -------- -------- -------- PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 364 GuestJYQC GuestNGCB -1 2 12 39 39 120 120 1 none (0:00) none 1 0 0\n"
seekMatchesAlreadyPosted'' = BS.pack "You are unregistered - setting to unrated.\nYour seek matches one already posted by GuestJYQC.\n\n<sr> 119\nfics% \nCreating: GuestJYQC (++++) GuestNGCB (++++) unrated blitz 2 12\n{Game 364 (GuestJYQC vs. GuestNGCB) Creating unrated blitz match.}\n\a\n<12> rnbqkbnr pppppppp -------- -------- -------- -------- PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 364 GuestJYQC GuestNGCB -1 2 12 39 39 120 120 1 none (0:00) none 1 0 0\n"
seekInfoBlock' = BS.pack "seekinfo set.\n<sc>\n<s> 16 w=CatNail ti=02 rt=1997  t=3 i=0 r=u tp=suicide c=? rr=0-9999 a=f f=f\n<s> 44 w=masheen ti=02 rt=2628  t=5 i=0 r=u tp=suicide c=? rr=0-9999 a=t f=f\n<s> 51 w=masheen ti=02 rt=2628  t=2 i=12 r=u tp=suicide c=? rr=0-9999 a=t f=f\n<s> 81 w=GuestHZLT ti=01 rt=0P t=2 i=0 r=u tp=lightning c=? rr=0-9999 a=t f=f\n"
playMsg = BS.pack "Creating: GuestCCFP (++++) GuestGVJK (++++) unrated blitz 0 20 {Game 132 (GuestCCFP vs. GuestGVJK) Creating unrated blitz match.} <12> rnbqkbnr pppppppp ———— ———— ———— ———— PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 132 GuestCCFP GuestGVJK -1 0 20 39 39 10 10 1 none (0:00) none 1 0 0"
obs = BS.pack "You are now observing game 157.Game 157: IMUrkedal (2517) GMRomanov (2638) unrated standard 120 0<12> -------- -pp-Q--- pk------ ----p--- -P---p-- --qB---- -------- ---R-K-- B -1 0 0 0 0 9 157 IMUrkedal GMRomanov 0 120 0 18 14 383 38 57 K/e1-f1 (0:03) Kf1 0 0 0"
guestLoginTxt = BS.pack $ "Press return to enter the server as \"FOOBAR\":"

challenge' = BS.pack "Challenge: GuestYWYK (----) GuestMGSD (----) unrated blitz 2 12."
matchMsg = BS.pack "{Game 537 (GuestWSHB vs. GuestNDKP) Creating unrated blitz match.}"
gameResult'''' = BS.pack  "{Game 368 (ALTOTAS vs. CalicoCat) CalicoCat resigns} 1-0"
gameResult'' = BS.pack "\n{Game 406 (GuestQLHT vs. GuestVYVJ) GuestQLHT resigns} 0-1\n\nNo ratings adjustment done."
gameResult''' = BS.pack "{Game 181 (Danimateit vs. WhatKnight) Danimateit forfeits on time} 0-1"
drawOffered' = BS.pack "GuestDWXY offers you a draw."
