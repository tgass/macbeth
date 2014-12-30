
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
 CommandMsg (..),
 parseCommandMsg
) where

import Seek
import PositionParser

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

data CommandMsg =  ObserveMsg { head :: CommandHead
                                  , position :: Position }
                      | GamesMsg { head :: CommandHead
                                  , message :: BS.ByteString }
                      | SoughtMsg { head :: CommandHead
                                  , message :: BS.ByteString }
                      | AcknoledgeMessage { head :: CommandHead }
                      | PositionMessage { position :: Position }
                      | LoginMessage
                      | PasswordMessage
                      | LoggedInMessage
                      | PromptMessage
                      | TextMessage { message :: BS.ByteString } deriving (Show)

data CommandHead = CommandHead { commandId :: Int } deriving (Show)

parseCommandMsg :: BS.ByteString -> Either String CommandMsg
parseCommandMsg str = parseOnly parser str
                          where parser = choice [ observeMsg
                                                , gamesMsg
                                                , soughtMsg
                                                , parseAcknoledge
                                                , parsePrompt
                                                , parseMove
                                                , parseLogin
                                                , parsePassword
                                                , parseLoggedIn]

obs = BS.pack "You are now observing game 157.Game 157: IMUrkedal (2517) GMRomanov (2638) unrated standard 120 0<12> -------- -pp-Q--- pk------ ----p--- -P---p-- --qB---- -------- ---R-K-- B -1 0 0 0 0 9 157 IMUrkedal GMRomanov 0 120 0 18 14 383 38 57 K/e1-f1 (0:03) Kf1 0 0 0"


observeMsg :: Parser CommandMsg
observeMsg = do
                head <- commandHead 80
                position <- obsBody
                takeTill (== chr 23)
                char $ chr 23
                return $ ObserveMsg head position

obsBody :: Parser Position
obsBody = do
                takeTill (== '>')
                char '>'
                space
                p <- A.take 71
                return $ parsePosition $ BS.unpack p


gamesMsg :: Parser CommandMsg
gamesMsg = do
                head <- commandHead 43
                message <- takeTill (== chr 23)
                char $ chr 23
                return $ GamesMsg head message


soughtMsg :: Parser CommandMsg
soughtMsg = do
                head <- commandHead 157
                message <- takeTill (== chr 23)
                char $ chr 23
                return $ SoughtMsg head message


parseMove :: Parser CommandMsg
parseMove = do
              "<12>"
              space
              p <- A.take 71
              return $ PositionMessage $ parsePosition $ BS.unpack p

parseLogin :: Parser CommandMsg
parseLogin = "login: " >> return LoginMessage


parsePassword :: Parser CommandMsg
parsePassword = "password: " >> return PasswordMessage


parseLoggedIn :: Parser CommandMsg
parseLoggedIn = "**** Starting FICS session as Schoon ****" >> return LoggedInMessage


parsePrompt :: Parser CommandMsg
parsePrompt = "fics% " >> return PromptMessage


parseAcknoledge :: Parser CommandMsg
parseAcknoledge = do
                    head <- commandHead 159
                    char $ chr 23
                    return $ AcknoledgeMessage head


commandHead :: Int -> Parser CommandHead
commandHead code = do
                char $ chr 21
                id <- decimal
                char $ chr 22
                string $ BS.pack $ show code
                char $ chr 22
                return $ CommandHead id
