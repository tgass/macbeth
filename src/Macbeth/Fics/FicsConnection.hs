{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.FicsConnection (
  ficsConnection
) where

import Macbeth.Api.CommandMsg hiding (gameId)
import Macbeth.Api.Move
import Macbeth.Fics.Parsers.CommandMsgParser

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
import System.Directory
import System.FilePath
import System.Locale
import System.IO

import qualified Data.ByteString.Char8 as BS


data HelperState = HelperState { gameId' :: Maybe Int, isPlaying :: Bool }


ficsConnection :: IO (Handle, Chan CommandMsg)
ficsConnection = runResourceT $ do
  chan <- liftIO newChan
  (_, hsock) <- allocate (connectTo "freechess.org" $ PortNumber 5000) hClose
  liftIO $ hSetBuffering hsock LineBuffering
  resourceForkIO $ liftIO $ chain hsock chan
  return (hsock, chan)


chain :: Handle -> Chan CommandMsg -> IO ()
chain h chan = flip evalStateT (HelperState Nothing False) $ transPipe lift
  (sourceHandle h) $$
  fileLoggerC =$
  toCharC =$
  blockC (False, BS.empty) =$
  parseC =$
  extractC =$
  loggingC =$
  sink chan


sink :: Chan CommandMsg -> Sink CommandMsg (StateT HelperState IO) ()
sink chan = awaitForever $ liftIO . writeChan chan


loggingC :: Conduit CommandMsg (StateT HelperState IO) CommandMsg
loggingC = awaitForever $ \cmd -> liftIO (printCmdMsg cmd) >> yield cmd


extractC :: Conduit CommandMsg (StateT HelperState IO) CommandMsg
extractC = awaitForever $ \cmd -> do
  state <- get
  case cmd of
    GameCreation id _ -> put (state {gameId' = Just id}) >> sourceList []
    GameMove move
      | isCheckmate move && isOponentMove move -> sourceList $ cmd : [toGameResult move]
      | isNewGameUser move && fromMaybe False ((== gameId move) <$> gameId' state) -> do
          put (state {gameId' = Nothing }); sourceList [MatchAccepted move]
      | otherwise -> sourceList [cmd]
    Boxed cmds -> sourceList cmds
    MatchAccepted move -> if isPlaying state
      then sourceList [GameMove move]
      else do put (state {isPlaying = True}); sourceList [cmd]
    g@GameResult {} -> do put (state {isPlaying = False}); sourceList [g]
    _ -> yield cmd


parseC :: Conduit BS.ByteString (StateT HelperState IO) CommandMsg
parseC = awaitForever $ \str ->case parseCommandMsg str of
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
fileLoggerC = awaitForever $ \chunk -> liftIO (logFile chunk) >> yield chunk


toGameResult :: Move -> CommandMsg
toGameResult move = GameResult id reason result
  where (id, reason, result) = toGameResultTuple move


printCmdMsg :: CommandMsg -> IO ()
printCmdMsg Prompt = return ()
printCmdMsg NewSeek {} = return ()
printCmdMsg RemoveSeeks {} = return ()
printCmdMsg cmd = print cmd


logFile :: BS.ByteString -> IO ()
logFile chunk = do
  rootDir <- getUserDocumentsDirectory
  createDirectoryIfMissing False $ rootDir </> "Macbeth"
  dateTime <- fmap (formatTime defaultTimeLocale "%Y-%m-%d %H-%M-%S: ") getZonedTime
  appendFile (rootDir </> "Macbeth" </> "macbeth.log") $
    (foldr (.) (showString dateTime) $ fmap showLitChar (BS.unpack chunk)) "\n"

