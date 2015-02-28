
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
import Move
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
import Data.List.Split (splitOn)


type Position = [(Square, Piece)]

parseCommandMsg :: BS.ByteString -> Either String CommandMsg
parseCommandMsg str = parseOnly parser str where
  parser = choice [
                    parseMoveMsg
                  , confirmMoveMsg

                  , observeMsg
                  , gamesMsg
                  , soughtMsg

                  , acceptGameMsg
                  , matchMsg

                  , parseGameResult

                  , parseLogin
                  , parseAcknoledge
                  , parseSettingsDone
                  , parsePassword
                  , parseGuestLogin
                  , parseUnkownUsername
                  , parseInvalidPassword
                  , parseLoggedIn
                  , parsePrompt

                  ]


observeMsg :: Parser CommandMsg
observeMsg = do
  head <- commandHead 80
  move <- obsBody
  takeTill (== chr 23)
  char $ chr 23
  return $ Observe move


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
  return $ Games gL


acceptGameMsg :: Parser CommandMsg
acceptGameMsg = do
  head <- commandHead 11
  move <- obsBody
  takeTill (== chr 23)
  char $ chr 23
  return $ Accept move


playSuccessMsg :: Parser CommandMsg
playSuccessMsg = do
  head <- commandHead 1111111
  move <- obsBody
  takeTill (== chr 23)
  char $ chr 23
  return $ PlaySuccess move


-- "\NAK3\SYN1\SYN\a\n<12> rnbqkbnr pppppppp -------- -------- ----P--- -------- PPPP-PPP RNBQKBNR B 4 1 1 1 1 0 217 GuestJZFG GuestKNSF -1 2 12 39 39 120 120 1 P/e2-e4 (0:00) e4 0 0 0\n\ETB"
confirmMoveMsg :: Parser CommandMsg
confirmMoveMsg = do
  head <- commandHead 1
  move <- obsBody
  takeTill (== chr 23)
  char $ chr 23
  return $ ConfirmMove move


-- | ie: {Game 537 (GuestWSHB vs. GuestNDKP) Creating unrated blitz match.}
matchMsg :: Parser CommandMsg
matchMsg = do
  "{Game "
  id <- decimal
  space
  "("
  name1 <- manyTill anyChar space
  "vs. "
  name2 <- manyTill anyChar ")"
  manyTill anyChar "}"
  return $ Match id


soughtMsg :: Parser CommandMsg
soughtMsg = do
  head <- commandHead 157
  sL <- soughtList'
  char $ chr 23
  return $ Sought sL


parseMoveMsg :: Parser CommandMsg
parseMoveMsg = parseMove >>= \move -> return $ CommandMsg.Move move


parseLogin :: Parser CommandMsg
parseLogin = "login: " >> return Login


parsePassword :: Parser CommandMsg
parsePassword = "password: " >> return Password


parseGuestLogin :: Parser CommandMsg
parseGuestLogin = do
  "Press return to enter the server as \""
  name <- manyTill anyChar "\""
  ":"
  return $ GuestLogin name


parseUnkownUsername :: Parser CommandMsg
parseUnkownUsername = do
  "\""
  name <- manyTill anyChar "\""
  " is not a registered name.  You may use this name to play unrated games."
  return $ UnkownUsername name


-- | Beware the guest handles: ie GuestXWLW(U)
parseLoggedIn :: Parser CommandMsg
parseLoggedIn = do
  "**** Starting FICS session as "
  name <- manyTill anyChar space
  "****"
  return $ LoggedIn (Prelude.head $ splitOn "(" name)


parseInvalidPassword :: Parser CommandMsg
parseInvalidPassword = "**** Invalid password! ****" >> return InvalidPassword


parsePrompt :: Parser CommandMsg
parsePrompt = "fics% " >> return Prompt


parseAcknoledge :: Parser CommandMsg
parseAcknoledge = do
  head <- commandHead 519
  char $ chr 23
  return $ Acknoledge

parseSettingsDone = (char $ chr 23) >> return SettingsDone

commandHead :: Int -> Parser CommandHead
commandHead code = do
  char $ chr 21
  id <- decimal
  char $ chr 22
  string $ BS.pack $ show code
  char $ chr 22
  return $ CommandHead id


-- test data
playMsg = BS.pack "Creating: GuestCCFP (++++) GuestGVJK (++++) unrated blitz 0 20 {Game 132 (GuestCCFP vs. GuestGVJK) Creating unrated blitz match.} <12> rnbqkbnr pppppppp ———— ———— ———— ———— PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 132 GuestCCFP GuestGVJK -1 0 20 39 39 10 10 1 none (0:00) none 1 0 0"
matchMsgS = BS.pack "{Game 537 (GuestWSHB vs. GuestNDKP) Creating unrated blitz match.}"
obs = BS.pack "You are now observing game 157.Game 157: IMUrkedal (2517) GMRomanov (2638) unrated standard 120 0<12> -------- -pp-Q--- pk------ ----p--- -P---p-- --qB---- -------- ---R-K-- B -1 0 0 0 0 9 157 IMUrkedal GMRomanov 0 120 0 18 14 383 38 57 K/e1-f1 (0:03) Kf1 0 0 0"
guestLogin = BS.pack $ "Press return to enter the server as \"FOOBAR\":"
