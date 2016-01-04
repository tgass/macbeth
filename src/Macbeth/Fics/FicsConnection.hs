{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.FicsConnection (
  ficsConnection
) where

import Macbeth.Fics.Configuration
import Macbeth.Fics.FicsMessage hiding (gameId)
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Parsers.FicsMessageParser

import Control.Applicative
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Char
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import Data.Maybe
import Data.Time
import Network
import System.FilePath
import System.Locale
import System.IO

import qualified Data.ByteString.Char8 as BS


data HelperState = HelperState { config :: Config
                               , gameId' :: Maybe Int
                               , isPlaying :: Bool }


ficsConnection :: IO (Handle, Chan FicsMessage)
ficsConnection = runResourceT $ do
  (_, hsock) <- allocate (connectTo "freechess.org" $ PortNumber 5000) hClose
  liftIO $ hSetBuffering hsock LineBuffering
  config <- liftIO loadConfig
  chan <- liftIO newChan
  resourceForkIO $ liftIO $ chain hsock config chan
  return (hsock, chan)


chain :: Handle -> Config-> Chan FicsMessage -> IO ()
chain h config chan = flip evalStateT (HelperState config Nothing False) $ transPipe lift
  (sourceHandle h) $$
  fileLoggerC =$
  toCharC =$
  blockC (False, BS.empty) =$
  parseC =$
  extractC =$
  loggingC =$
  sink chan


sink :: Chan FicsMessage -> Sink FicsMessage (StateT HelperState IO) ()
sink chan = awaitForever $ liftIO . writeChan chan


loggingC :: Conduit FicsMessage (StateT HelperState IO) FicsMessage
loggingC = awaitForever $ \cmd -> do
  logToStdOut <- (stdOut . logging . config) `fmap` get
  when logToStdOut $ liftIO (printCmdMsg cmd)
  yield cmd


extractC :: Conduit FicsMessage (StateT HelperState IO) FicsMessage
extractC = awaitForever $ \cmd -> do
  state <- get
  case cmd of
    Boxed cmds -> sourceList cmds
    GameCreation id _ -> put (state {gameId' = Just id}) >> sourceList []
    m@(GameMove _ move)
      | isCheckmate move && isOponentMove move -> sourceList $ cmd : [toGameResult move]
      | isNewGameUser move && fromMaybe False ((== gameId move) <$> gameId' state) -> do
          put (state {gameId' = Nothing })
          sourceList [MatchAccepted move]
      | otherwise -> sourceList [m]
    MatchAccepted move
      | isPlaying state -> sourceList [GameMove (Just $ Takeback $ nameOponent move) move]
      | otherwise -> do put (state {isPlaying = True}); sourceList [cmd]
    g@GameResult {} -> do
      put (state {isPlaying = False})
      sourceList [g]
    _ -> yield cmd


parseC :: Conduit BS.ByteString (StateT HelperState IO) FicsMessage
parseC = awaitForever $ \str -> case parseFicsMessage str of
  Left _    -> yield $ TextMessage $ BS.unpack str
  Right cmd -> yield cmd


blockC :: (Bool, BS.ByteString) -> Conduit Char (StateT HelperState IO) BS.ByteString
blockC (block, p) = awaitForever $ \c -> case p of
  "login:" -> yield "login: " >> blockC (False, BS.empty)
  "password:" -> yield "password: " >> blockC (False, BS.empty)
  _ -> case ord c of
    21 -> blockC (True, BS.singleton c)
    23 -> yield (p `BS.append` BS.singleton c) >> blockC (False, BS.empty)
    10 -> if block then blockC (block, p `BS.append` BS.singleton c)
                   else when(p /= "") $ yield p >> blockC (block, BS.empty)
    13 -> blockC (block, p) -- ignores \r
    _  -> blockC (block, p `BS.append` BS.singleton c)


toCharC :: Conduit BS.ByteString (StateT HelperState IO) Char
toCharC = awaitForever $ sourceList . BS.unpack


fileLoggerC :: Conduit BS.ByteString (StateT HelperState IO) BS.ByteString
fileLoggerC = awaitForever $ \chunk -> do
   config <- config `fmap` get
   when (file (logging config) && isJust (directory config)) $
     liftIO $ logFile (fromJust $ directory config) chunk
   yield chunk


toGameResult :: Move -> FicsMessage
toGameResult move = GameResult id reason result
  where (id, reason, result) = toGameResultTuple move


printCmdMsg :: FicsMessage -> IO ()
printCmdMsg Prompt = return ()
printCmdMsg NewSeek {} = return ()
printCmdMsg RemoveSeeks {} = return ()
printCmdMsg cmd = print cmd


logFile :: FilePath -> BS.ByteString -> IO ()
logFile path chunk = do
  date <- fmap (formatTime defaultTimeLocale "%Y-%m-%d_") getZonedTime
  dateTime <- fmap (formatTime defaultTimeLocale "%Y-%m-%d %H-%M-%S: ") getZonedTime
  appendFile (path </> date ++ "macbeth.log") $
    (foldr (.) (showString $ "\n" ++ dateTime) $ fmap showLitChar (BS.unpack chunk)) ""

