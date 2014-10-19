{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as BS
import Network (connectTo, PortID (..))
import System.IO
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WX.Events
import Graphics.UI.WX.Timer
import Graphics.UI.WX.Types
import Graphics.UI.WXCore.WxcDefs
import Data.List.Split
import Data.Text.Encoding
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)

myEventId = wxID_HIGHEST+1 -- the custom event ID
-- the custom event is registered as a menu event
createMyEvent = commandEventCreate wxEVT_COMMAND_MENU_SELECTED myEventId
registerMyEvent win io = evtHandlerOnMenuCommand win myEventId io

main = start gui

gui = do
  mv <- newEmptyMVar
  f <- frame []
  t <- textCtrlEx f (wxTE_MULTILINE .+. wxTE_RICH) [font := fontFixed]
  e <- entry f []
  set f [layout := boxed "console" (grid 5 5 [[floatLeft $ expand $ hstretch $ widget t]
                                             ,[expand $ hstretch $ widget e]])]
  registerMyEvent t (updateConsoleContent mv t)
  telnet "freechess.org" 5000 t e mv

updateConsoleContent :: MVar String -> TextCtrl () -> IO ()
updateConsoleContent mv t = do
  str <- takeMVar mv
  appendText t (str ++ "\n")
  return ()

telnet :: String -> Int -> TextCtrl() -> TextCtrl() -> MVar String -> IO ()
telnet host port t e mv = runResourceT $ do
    (releaseSock, hsock) <- allocate (connectTo host $ PortNumber $ fromIntegral port) hClose
    liftIO $ hSetBuffering hsock LineBuffering
    liftIO $ set e [on enterKey := emitCommand t e hsock]
    resourceForkIO (liftIO $ CB.sourceHandle hsock $= splitLines $$ (sink' hsock t mv))
    return ()

splitLines :: Conduit BS.ByteString IO BS.ByteString
splitLines = awaitForever $ \str -> CL.sourceList $ filter (/= "fics% ") $ tokenise "\n\r" str
  where
    tokenise :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
    tokenise x y = h : if BS.null t then [] else tokenise x (BS.drop (BS.length x) t)
      where (h,t) = BS.breakSubstring x y

sink' :: Handle -> TextCtrl () -> MVar String -> Sink BS.ByteString IO ()
sink' h textCtrl mv = awaitForever $ \str -> do
    liftIO $ putMVar mv (BS.unpack str)
    ev <- liftIO $ createMyEvent
    liftIO $ evtHandlerAddPendingEvent textCtrl ev
    case str of
      "login: " -> liftIO $ hPutStrLn h "Schoon"
      "password: " -> liftIO $ hPutStrLn h "*****"
      "**** Starting FICS session as Schoon ****" -> liftIO $ hPutStrLn h "set seek 0"
      _ -> return ()

emitCommand :: TextCtrl () -> TextCtrl () -> Handle -> IO ()
emitCommand t e h = do
  command <- get e text
  appendText t command
  set e [text := ""]
  hPutStrLn h command

