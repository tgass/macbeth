{-# LANGUAGE OverloadedStrings #-}

module FicsConnection (
  ficsConnection
) where

import CommandMsg
import CommandMsgParser
import WxSink

import Control.Concurrent.Chan
import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Network (connectTo, PortID (..))

import System.Directory
import System.FilePath
import System.IO


ficsConnection :: IO (Handle, Chan CommandMsg)
ficsConnection = runResourceT $ do
  chan <- liftIO newChan
  (releaseSock, hsock) <- allocate (connectTo "freechess.org" $ PortNumber 5000) hClose
  liftIO $ hSetBuffering hsock LineBuffering
  resourceForkIO $ liftIO $ chain (wxSink chan) hsock
  return (hsock, chan)


chain :: (Handle -> CommandMsg -> IO ()) -> Handle -> IO ()
chain handler hsock =
  CB.sourceHandle hsock $$
  toCharC =$
  blockC (False, BS.empty) =$
  parseC =$
  sink (handler hsock)


sink :: (CommandMsg -> IO ()) -> Sink CommandMsg IO ()
sink handler = awaitForever $ liftIO . handler


parseC :: Conduit BS.ByteString IO CommandMsg
parseC = awaitForever $ \str -> case parseCommandMsg str of
                                  Left _    -> yield $ TextMessage $ BS.unpack str
                                  Right cmd -> yield cmd


blockC :: (Bool, BS.ByteString) -> Conduit Char IO BS.ByteString
blockC (block, p) = awaitForever $ \c -> do
                                    liftIO $ logger c
                                    case p of
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


logger :: Char -> IO ()
logger c = do
  rootDir <- getUserDocumentsDirectory
  createDirectoryIfMissing False $ rootDir </> "XChess"
  appendFile (rootDir </> "XChess" </> "xchess.log") [c]

