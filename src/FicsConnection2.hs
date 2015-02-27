{-# LANGUAGE OverloadedStrings #-}

module FicsConnection2 (
  ficsConnection
) where

import CommandMsg
import CommandMsgParser

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Network (connectTo, PortID (..))
import System.IO


main :: IO ()
main = ficsConnection handler >>= loop


loop :: Handle -> IO ()
loop h = getLine >>= hPutStrLn h >> loop h


handler :: Handle -> CommandMsg -> IO ()
handler h cmd = case cmd of
      LoginMessage       -> hPutStrLn h "guest"

      PasswordMessage    -> hPutStrLn h ""

      LoggedInMessage _  -> hPutStrLn h "set seek 0" >>
                            hPutStrLn h "set style 12" >>
                            hPutStrLn h "iset nowrap 1" >>
                            hPutStrLn h "iset block 1"

      GuestLoginMsg _    -> hPutStrLn h ""

      _ -> putStrLn $ show cmd



ficsConnection :: (Handle -> CommandMsg -> IO ()) -> IO (Handle)
ficsConnection handler = runResourceT $ do
                          (releaseSock, hsock) <- allocate (connectTo "freechess.org" $ PortNumber $ fromIntegral 5000) hClose
                          liftIO $ hSetBuffering hsock LineBuffering
                          resourceForkIO $ liftIO $
                            CB.sourceHandle hsock $$ toCharC =$ blockC (False, BS.empty) =$ parseC =$ sink (handler hsock)
                          return hsock


sink :: (CommandMsg -> IO ()) -> Sink CommandMsg IO ()
sink handler = awaitForever $ liftIO . handler



parseC :: Conduit BS.ByteString IO CommandMsg
parseC = awaitForever $ \str -> case parseCommandMsg str of
                                  Left _    -> yield (TextMessage str) >> parseC
                                  Right msg -> yield msg >> parseC


blockC :: (Bool, BS.ByteString) -> Conduit Char IO BS.ByteString
blockC (block, p) = awaitForever $ \c -> do
                                    case p of
                                      "login:" -> yield "login: " >> blockC (False, BS.empty)
                                      "password:" -> yield "password: " >> blockC (False, BS.empty)
                                      _ -> case ord c of
                                            21 -> blockC (True, BS.singleton c)
                                            23 -> yield (p `BS.append` (BS.singleton c)) >> blockC (False, BS.empty)
                                            10 -> if block then blockC (block, (p `BS.append` BS.singleton c))
                                                           else yield p >> blockC (block, BS.empty)
                                            13 -> blockC (block, p) -- ignores \r
                                            _  -> blockC (block, p `BS.append` (BS.singleton c))


toCharC :: Conduit BS.ByteString IO Char
toCharC = awaitForever $ CL.sourceList . BS.unpack


