
{-

<BLOCK_START><command identifier><BLOCK_SEPARATOR>

This will be followed by:

<command code><BLOCK_SEPARATOR><command output><BLOCK_END>

#define BLOCK_START 21 /* '\U' */
#define BLOCK_SEPARATOR 22 /* '\V' */
#define BLOCK_END 23 /* '\W' */
#define BLOCK_POSE_START 24 /* \X */
#define BLOCK_POSE_END 25 /* \Y */

-}

{-
CommandMessage {commandId = 3, commandCode = 80, message = "You are now observing game 157.Game 157: IMUrkedal (2517) GMRomanov (2638) unrated standard 120 0<12> -------- -pp-Q--- pk------ ----p--- -P---p-- --qB---- -------- ---R-K-- B -1 0 0 0 0 9 157 IMUrkedal GMRomanov 0 120 0 18 14 383 38 57 K/e1-f1 (0:03) Kf1 0 0 0"}
-}

{-# LANGUAGE OverloadedStrings #-}

module CommandMsgParser (
 parseCommandMsg
) where

import Api
import CommandMsg

import SeekParser
import GamesParser
import MoveParser2
import GameResultParser

import Control.Applicative ((<*>), (*>), (<*), (<$>), (<|>), pure)

import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as A (take, takeWhile)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.List as List (filter)
import Data.Maybe (fromMaybe)

import Network (connectTo, PortID (..))
import System.IO

type Position = [(Square, Piece)]



parseCommandMsg :: BS.ByteString -> Either String CommandMsg
parseCommandMsg str = parseOnly parser str where
  parser = choice [ observeMsg
                  , gamesMsg
                  , soughtMsg
                  , acceptGameMsg
                  , matchMsg
                  , parseGameResult
                  , parseAcknoledge
                  , parsePrompt
                  , parseMoveMsg
                  , parseLogin
                  , parseSettingsDone
                  , parsePassword
                  , parseGuestLogin
                  , parseUnkownUsername
                  , parseInvalidPassword
                  , parseLoggedIn]



observeMsg :: Parser CommandMsg
observeMsg = do
  head <- commandHead 80
  move <- obsBody
  takeTill (== chr 23)
  char $ chr 23
  return $ ObserveMsg head move


obsBody :: Parser Move
obsBody = do
  takeTill (== '<')
  move <- parseMove
  return move


gamesMsg :: Parser CommandMsg
gamesMsg = do
  head <- commandHead 43
  gL <- paresGamesList
  char $ chr 23
  return $ GamesMsg head gL


acceptGameMsg :: Parser CommandMsg
acceptGameMsg = do
  head <- commandHead 11
  move <- obsBody
  takeTill (== chr 23)
  char $ chr 23
  return $ AcceptMsg move


-- | ie: {Game 537 (GuestWSHB vs. GuestNDKP) Creating unrated blitz match.}
matchMsg :: Parser CommandMsg
matchMsg = do
  "{Game "
  id <- decimal
  space
  takeTill (=='}')
  "}"
  return $ MatchMsg id



soughtMsg :: Parser CommandMsg
soughtMsg = do
  head <- commandHead 157
  sL <- soughtList'
  char $ chr 23
  return $ SoughtMsg head sL


parseMoveMsg :: Parser CommandMsg
parseMoveMsg = parseMove >>= \move -> return $ MoveMsg move


parseLogin :: Parser CommandMsg
parseLogin = "login: " >> return LoginMessage


parsePassword :: Parser CommandMsg
parsePassword = "password: " >> return PasswordMessage


parseGuestLogin :: Parser CommandMsg
parseGuestLogin = do
  "Press return to enter the server as \""
  name <- manyTill anyChar "\""
  ":"
  return $ GuestLoginMsg name


parseUnkownUsername :: Parser CommandMsg
parseUnkownUsername = do
  "\""
  name <- manyTill anyChar "\""
  " is not a registered name.  You may use this name to play unrated games."
  return $ UnkownUsernameMsg name


parseLoggedIn :: Parser CommandMsg
parseLoggedIn = do
  "**** Starting FICS session as "
  name <- manyTill anyChar space
  "****"
  return LoggedInMessage


parseInvalidPassword :: Parser CommandMsg
parseInvalidPassword = "**** Invalid password! ****" >> return InvalidPasswordMsg


parsePrompt :: Parser CommandMsg
parsePrompt = "fics% " >> return PromptMessage


parseAcknoledge :: Parser CommandMsg
parseAcknoledge = do
  head <- commandHead 519
  char $ chr 23
  return $ AcknoledgeMessage head

parseSettingsDone = (char $ chr 23) >> return SettingsDoneMsg

commandHead :: Int -> Parser CommandHead
commandHead code = do
  char $ chr 21
  id <- decimal
  char $ chr 22
  string $ BS.pack $ show code
  char $ chr 22
  return $ CommandHead id


-- test data
obs = BS.pack "You are now observing game 157.Game 157: IMUrkedal (2517) GMRomanov (2638) unrated standard 120 0<12> -------- -pp-Q--- pk------ ----p--- -P---p-- --qB---- -------- ---R-K-- B -1 0 0 0 0 9 157 IMUrkedal GMRomanov 0 120 0 18 14 383 38 57 K/e1-f1 (0:03) Kf1 0 0 0"
guestLogin = BS.pack $ "Press return to enter the server as \"FOOBAR\":"
