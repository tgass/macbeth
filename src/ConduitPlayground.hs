{-# LANGUAGE OverloadedStrings #-}

module Connect (
  main
) where

import Seek

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
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as A (take, takeWhile)

import Control.Applicative ((<*>), (*>), (<*), (<$>), (<|>), pure)


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
                                                  CB.sourceHandle hsock $$ conduit =$ conduit2 BS.empty =$ sink
                                                  return ()
                          return hsock


sink :: Sink CommandMessage IO ()
sink = awaitForever $ \command -> liftIO $ putStrLn $ show command


--conduit2 :: Monad m => Maybe (Result CommandMessage) -> Conduit BS.ByteString m CommandMessage
conduit2 :: BS.ByteString -> Conduit BS.ByteString IO CommandMessage
conduit2 partial = awaitForever $ \str -> do

                         liftIO $ putStrLn $ show partial
                         let result' = parseCommandMessage (partial `BS.append` str)

                         case result' of
                           Fail _ _ _    -> yield $ TextMessage str
                           Partial _     -> conduit2 $ (partial `BS.append` str)
                           Done rest msg -> yield msg


conduit :: Monad m => Conduit BS.ByteString m BS.ByteString
conduit = awaitForever $ \str -> CL.sourceList $ tokenise "\n\r" str


tokenise :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
tokenise x y = h : if BS.null t then [] else tokenise x (BS.drop (BS.length x) t)
               where (h,t) = BS.breakSubstring x y







