{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.FicsConnection (
  ficsConnection
) where

import Macbeth.Api.CommandMsg hiding (gameId)
import Macbeth.Api.Move
import Macbeth.Fics.Parsers.CommandMsgParser

import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Char
import Data.Maybe
import Network (connectTo, PortID (..))
import System.IO (Handle, BufferMode (..), hSetBuffering, hClose)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

data HelperState = HelperState { gameId' :: Maybe Int }

ficsConnection :: IO (Handle, Chan CommandMsg)
ficsConnection = runResourceT $ do
  chan <- liftIO newChan
  (_, hsock) <- allocate (connectTo "freechess.org" $ PortNumber 5000) hClose
  liftIO $ hSetBuffering hsock LineBuffering
  resourceForkIO $ liftIO $ chain hsock chan
  return (hsock, chan)


chain :: Handle -> Chan CommandMsg -> IO ()
chain h chan = flip evalStateT (HelperState Nothing) $ transPipe lift
  (CB.sourceHandle h) $$
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
    m@(GameMove move) -> CL.sourceList $ foo (gameId' state) m move
    Boxed cmds -> CL.sourceList cmds
    GameCreation id _ -> put (HelperState $ Just id) >> CL.sourceList []
    _ -> yield cmd

foo :: Maybe Int -> CommandMsg -> Move -> [CommandMsg]
foo mId cmd move
  | isCheckmate move && relation move == OponentsMove = cmd : [toGameResult move]
  | isNewGameUser move && isJust mId = if fromJust mId == gameId move then [MatchAccepted move] else [cmd]
  | otherwise = [cmd]

parseC :: (Monad m) => Conduit BS.ByteString m CommandMsg
parseC = awaitForever $ \str -> case parseCommandMsg str of
  Left _    -> yield $ TextMessage $ BS.unpack str
  Right cmd -> yield cmd


blockC :: (Monad m) => (Bool, BS.ByteString) -> Conduit Char m BS.ByteString
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


toCharC :: (Monad m) => Conduit BS.ByteString m Char
toCharC = awaitForever $ CL.sourceList . BS.unpack


toGameResult :: Move -> CommandMsg
toGameResult move = GameResult id reason result
  where (id, reason, result) = Macbeth.Api.Move.toGameResultTuple move

printCmdMsg :: CommandMsg -> IO ()
printCmdMsg Prompt = return ()
printCmdMsg (NewSeek _) = return ()
printCmdMsg (RemoveSeeks _) = return ()
printCmdMsg cmd = print cmd

