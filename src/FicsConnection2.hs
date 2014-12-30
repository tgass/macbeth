{-# LANGUAGE OverloadedStrings #-}

module FicsConnection2 (
  main,
  ficsConnection
) where

import Seek
import PositionParser
import CommandMsgParser

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
import Data.Word

import Network (connectTo, PortID (..))
import System.IO


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


handler :: CommandMsg -> IO ()
handler msg = putStrLn $ show msg

ficsConnection :: (CommandMsg -> IO ()) -> IO (Handle)
ficsConnection handler = runResourceT $ do
                          (releaseSock, hsock) <- allocate
                                    (connectTo "freechess.org" $ PortNumber $ fromIntegral 5000) hClose
                          liftIO $ hSetBuffering hsock LineBuffering
                          resourceForkIO $ liftIO $
                            CB.sourceHandle hsock $$ blockY =$ blockX (False, BS.empty) =$ parseC =$ sink handler
                          return hsock


sink :: (CommandMsg -> IO ()) -> Sink CommandMsg IO ()
sink handler = awaitForever $ \command -> liftIO $ handler command


parseC :: Monad m => Conduit BS.ByteString m CommandMsg
parseC = awaitForever $ \str -> do
                                  case parseCommandMsg str of
                                     Left _    -> yield (TextMessage str) >> parseC
                                     Right msg -> yield msg >> parseC


blockX :: Monad m => (Bool, BS.ByteString) -> Conduit Char m BS.ByteString
blockX (block, p) = awaitForever $ \c -> do
                                    case p of
                                      "login:" -> yield "login: " >> blockX (False, BS.empty)
                                      "password:" -> yield "password: " >> blockX (False, BS.empty)
                                      _ -> case ord c of
                                            21 -> blockX (True, BS.singleton c)
                                            23 -> yield (p `BS.append` (BS.singleton c)) >> blockX (False, BS.empty)
                                            10 -> if block then blockX (block, (p `BS.append` BS.singleton c))
                                                           else yield p >> blockX (block, BS.empty)
                                            13 -> blockX (block, p) -- ignores \r
                                            _  -> blockX (block, p `BS.append` (BS.singleton c))


blockY :: Monad m => Conduit BS.ByteString m Char
blockY = awaitForever $ \str -> CL.sourceList $ BS.unpack str


loggingC :: Conduit BS.ByteString IO BS.ByteString
loggingC = awaitForever $ \str -> do
                            liftIO $ putStrLn $ show str
                            yield str

