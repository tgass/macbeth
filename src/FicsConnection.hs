{-# LANGUAGE OverloadedStrings #-}

module FicsConnection (
  getFicsHandle,
  registerFicsEvents
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as BS
import Network (connectTo, PortID (..))
import System.IO
import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Concurrent (MVar, takeMVar, putMVar)

ficsEventId = wxID_HIGHEST + 51

registerEvent win io = evtHandlerOnMenuCommand win ficsEventId io

getFicsHandle :: Frame () -> MVar BS.ByteString -> IO (Handle)
getFicsHandle f mvar = ficsConnection (fireEventSink f mvar) >>= return

ficsConnection :: (Sink BS.ByteString IO ()) -> IO (Handle)
ficsConnection sink = runResourceT $ do
                          (releaseSock, hsock) <- allocate
                                    (connectTo "freechess.org" $ PortNumber $ fromIntegral 5000) hClose
                          liftIO $ hSetBuffering hsock LineBuffering
                          resourceForkIO (liftIO $ CB.sourceHandle hsock $$ sink)
                          return hsock


fireEventSink :: Frame () -> MVar BS.ByteString -> Sink BS.ByteString IO ()
fireEventSink f mv = awaitForever $ \str -> do
                              liftIO $ putMVar mv str >>
                                       commandEventCreate wxEVT_COMMAND_MENU_SELECTED ficsEventId >>= \ev ->
                                       evtHandlerAddPendingEvent f ev


registerFicsEvents :: Frame () -> Handle -> MVar BS.ByteString -> [[BS.ByteString] -> IO ()] -> IO ()
registerFicsEvents f h mv ux = registerEvent f (processEvent mv $ basic ++ ux)
                      where basic = [login h, config h]


processEvent :: MVar BS.ByteString -> [[BS.ByteString] -> IO ()] -> IO ()
processEvent mv ux = takeMVar mv >>= \str -> runUpdates ux (tokenise "\n\r" str)
                          where
                            runUpdates :: [[BS.ByteString] -> IO()] -> [BS.ByteString] -> IO ()
                            runUpdates [] str = return ()
                            runUpdates (x:xs) str = x str >> runUpdates xs str


login :: Handle -> [BS.ByteString] -> IO ()
login h lines = if elem "login: " lines
        then hPutStrLn h "Schoon"
        else return ()


--password :: Handle -> [BS.ByteString] -> IO ()
--password h lines = if elem "password: " lines
--        then hPutStrLn h "*****"
--        else return ()


config :: Handle -> [BS.ByteString] -> IO ()
config h lines = if elem "**** Starting FICS session as Schoon ****" lines
        then hPutStrLn h "set seek 0" >> hPutStrLn h "sought"
        else return ()


tokenise :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
tokenise x y = h : if BS.null t then [] else tokenise x (BS.drop (BS.length x) t)
               where (h,t) = BS.breakSubstring x y

