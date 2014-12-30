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
                            CB.sourceHandle hsock $$ linesC =$ blockC Nothing =$ parseC =$ sink handler
                          return hsock


sink :: (CommandMsg -> IO ()) -> Sink CommandMsg IO ()
sink handler = awaitForever $ \command -> liftIO $ handler command


parseC :: Monad m => Conduit BS.ByteString m CommandMsg
parseC = awaitForever $ \str -> do
                                  case parseCommandMsg str of
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

