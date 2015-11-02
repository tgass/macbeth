{-# LANGUAGE OverloadedStrings #-}

module Lentils.Fics.FicsConnection (
  ficsConnection
) where

import Lentils.Api.CommandMsg
import Lentils.Api.Move
import Lentils.Fics.Parsers.CommandMsgParser

import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT, allocate, resourceForkIO)
import Data.Char
import Data.Conduit
import Network (connectTo, PortID (..))
import System.IO (Handle, BufferMode (..), hSetBuffering, hClose)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL


ficsConnection :: IO (Handle, Chan CommandMsg)
ficsConnection = runResourceT $ do
  chan <- liftIO newChan
  (_, hsock) <- allocate (connectTo "freechess.org" $ PortNumber 5000) hClose
  liftIO $ hSetBuffering hsock LineBuffering
  resourceForkIO $ liftIO $ chain hsock chan
  return (hsock, chan)


chain :: Handle -> Chan CommandMsg -> IO ()
chain h chan =
  CB.sourceHandle h $$
  toCharC =$
  blockC (False, BS.empty) =$
  parseC =$
  extractC =$
  loggingC =$
  sink chan


sink :: Chan CommandMsg -> Sink CommandMsg IO ()
sink chan = awaitForever $ liftIO . writeChan chan


loggingC :: Conduit CommandMsg IO CommandMsg
loggingC = awaitForever $ \cmd -> liftIO (printCmdMsg cmd) >> yield cmd


extractC :: Conduit CommandMsg IO CommandMsg
extractC = awaitForever $ \cmd -> case cmd of
  m@(GameMove move) -> CL.sourceList $ m : [toGameResult move | isCheckmate move && relation move == OponentsMove]
  Boxed cmds -> CL.sourceList cmds
  _ -> yield cmd


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
  where (id, reason, result) = Lentils.Api.Move.toGameResultTuple move

printCmdMsg :: CommandMsg -> IO ()
printCmdMsg Prompt = return ()
printCmdMsg (NewSeek _) = return ()
printCmdMsg (RemoveSeeks _) = return ()
printCmdMsg (GameMove m) = print $ prettyMove m
printCmdMsg cmd = print cmd

