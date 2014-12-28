{-# LANGUAGE OverloadedStrings #-}

module FicsConnection2 (
  main,
  ficsConnection,
  CommandMessage (..),
  parseLogin
) where

import Seek
import PositionParser

import Control.Applicative ((<*>), (*>), (<*), (<$>), (<|>), pure)
import Control.Concurrent (MVar, takeMVar, putMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource

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



data CommandMessage =  ObserveMsg { head :: CommandHead
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

main :: IO ()
main = do
  h <- ficsConnection handler
  loop h
  return ()

loop h = do
      line <- getLine
      case line of
        "quit" -> return ()
        _ -> hPutStrLn h line >> loop h


handler :: CommandMessage -> IO ()
handler msg = putStrLn $ show msg

ficsConnection :: (CommandMessage -> IO ()) -> IO (Handle)
ficsConnection handler = runResourceT $ do
                          (releaseSock, hsock) <- allocate
                                    (connectTo "freechess.org" $ PortNumber $ fromIntegral 5000) hClose
                          liftIO $ hSetBuffering hsock LineBuffering
                          resourceForkIO $ liftIO $
                            CB.sourceHandle hsock $$ linesC =$ blockC Nothing =$ parseC =$ sink handler
                          return hsock


sink :: (CommandMessage -> IO ()) -> Sink CommandMessage IO ()
sink handler = awaitForever $ \command -> liftIO $ handler command


parseC :: Monad m => Conduit BS.ByteString m CommandMessage
parseC = awaitForever $ \str -> do
                                  case parseCommandMessage str of
                                     Left _    -> yield (TextMessage str) >> parseC
                                     Right msg -> yield msg >> parseC


blockC :: Monad m => Maybe BS.ByteString -> Conduit BS.ByteString m BS.ByteString
blockC partial = awaitForever $ \str' -> do
                                let str = if BS.index str' 0 == '\r' then (BS.tail str') `BS.append` "\n" else str'
                                case ord $ BS.index str 0 of
                                  21 -> blockC $ Just str
                                  23 -> yield ((fromMaybe "" partial) `BS.append` str) >> blockC Nothing
                                  _  -> case partial of
                                        Just p -> blockC $ Just (p `BS.append` str)
                                        Nothing  -> yield str >> blockC Nothing


linesC :: Monad m => Conduit BS.ByteString m BS.ByteString
linesC = awaitForever $ \str -> CL.sourceList $ filter (flip notElem ["", "\r"]) $ tokenise "\n\r" str

{-
 * Strips only \n, but leaves as a result \r in front of all lines.
 * \r serves as marker for line breakes as tokenise does not always split complete lines
-}
tokenise :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
tokenise x y = h : if BS.null t then [] else tokenise x (BS.drop (BS.length x - 1) t)
               where (h,t) = BS.breakSubstring x y


loggingC :: Conduit BS.ByteString IO BS.ByteString
loggingC = awaitForever $ \str -> do
                            liftIO $ putStrLn $ show str
                            yield str


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

parseCommandMessage :: BS.ByteString -> Either String CommandMessage
parseCommandMessage str = parseOnly parser str
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


observeMsg :: Parser CommandMessage
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


gamesMsg :: Parser CommandMessage
gamesMsg = do
                head <- commandHead 43
                message <- takeTill (== chr 23)
                char $ chr 23
                return $ GamesMsg head message


soughtMsg :: Parser CommandMessage
soughtMsg = do
                head <- commandHead 157
                message <- takeTill (== chr 23)
                char $ chr 23
                return $ SoughtMsg head message


parseMove :: Parser CommandMessage
parseMove = do
              "<12>"
              space
              p <- A.take 71
              return $ PositionMessage $ parsePosition $ BS.unpack p

parseLogin :: Parser CommandMessage
parseLogin = "login: " >> return LoginMessage


parsePassword :: Parser CommandMessage
parsePassword = "password: " >> return PasswordMessage


parseLoggedIn :: Parser CommandMessage
parseLoggedIn = "**** Starting FICS session as Schoon ****" >> return LoggedInMessage


parsePrompt :: Parser CommandMessage
parsePrompt = "fics% " >> return PromptMessage


parseAcknoledge :: Parser CommandMessage
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
