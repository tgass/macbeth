{-# LANGUAGE OverloadedStrings #-}

module Connect (
  main
) where

import Seek
import SeekParser
import PositionParser

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as BS
import Network (connectTo, PortID (..))
import System.IO
import Control.Concurrent (MVar, takeMVar, putMVar)
import Data.Attoparsec.Char8


main :: IO ()
main = do
  h <- ficsConnection
  loop h
  return ()

loop h = do
      line <- getLine
      case line of
        "quit" -> return ()
        _ -> hPutStrLn h line >> loop h




ficsConnection :: IO (Handle)
ficsConnection = runResourceT $ do
                          (releaseSock, hsock) <- allocate
                                    (connectTo "freechess.org" $ PortNumber $ fromIntegral 5000) hClose
                          liftIO $ hSetBuffering hsock LineBuffering
                          resourceForkIO $ liftIO $ do
                                                  CB.sourceHandle hsock $$ sink BS.empty
                                                  return ()
                          return hsock


sink :: BS.ByteString -> Sink BS.ByteString IO ()
sink partial = awaitForever $ \str -> do
                         let lines' = partial `BS.append` str
                         let result = parseCommandMessage lines'
                         case result of
                           Fail _ _ _ -> do
                                          liftIO $ putStr $ "FAIL:\n" ++ BS.unpack lines'
                                          sink BS.empty
                           Partial _ -> do
                                        liftIO $ putStr $ "PARTIAL:\n"
                                        sink lines'
                           Done rest final -> do
                                              liftIO $ putStr $ "DONE:\n" ++ show final ++ " // \nREST:\n" ++ show rest
                                              sink BS.empty








