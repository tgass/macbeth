{-# LANGUAGE LambdaCase, ScopedTypeVariables, MultiWayIf #-}

module Macbeth.Wx.Chat (
  wxChat
) where

import Macbeth.Fics.FicsMessage hiding (gameId)
import Macbeth.Fics.Api.Chat
import Macbeth.Fics.Api.Player hiding (handle)
import Macbeth.Wx.Utils
import qualified Macbeth.Wx.RuntimeEnv as E
import qualified Macbeth.Wx.Config.UserConfig as C

import Control.Concurrent
import Control.Monad
import Control.Exception
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import System.IO

eventId = wxID_HIGHEST + 1

wxChat :: E.RuntimeEnv -> a -> [ChatMsg] -> Chan FicsMessage -> IO ()
wxChat env _ chatMsgs chan = do
  username <- E.username env
  vCmd <- newEmptyMVar

  f <- frame [ text := "Chat with " ++ username]
  windowSetFocus f
  p <- panel f []

  ct <- textCtrlEx p (wxTE_MULTILINE .+. wxTE_RICH .+. wxTE_READONLY) [font := fontFixed {_fontSize = env `E.getConfig` C.fontSize}]
  prefill ct chatMsgs

  ce <- entry p []
  set ce [on enterKey := tell ce username (E.handle env) chan]

  set f [layout := minsize (Size 400 200) $ container p $ margin 10 $
                     column 5 [ fill $ widget ct, hfill $ widget ce]]

  windowOnKeyDown p (\evt -> if
    | keyWithMod evt 'W' justControl -> close f
    | otherwise -> return ())

  threadId <- forkIO $ eventLoop eventId chan vCmd f
  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \case

    Chat msg -> when (msg `belongsTo` username) $ showMsg ct msg

    WxClose -> close f

    _ -> return ()

  windowOnDestroy f $ do
    writeChan chan $ Chat $ CloseChat username
    sequence_ $ fmap (handle (\(_ :: IOException) -> return ()) . killThread) [threadId]


prefill :: TextCtrl () -> [ChatMsg] -> IO ()
prefill _ [] = return ()
prefill ct (m:mx) = showMsg ct m >> prefill ct mx


tell :: TextCtrl () -> Username -> Handle -> Chan FicsMessage -> IO ()
tell ce username h chan = do
  msg <- get ce text
  hPutStrLn h $ "7 tell " ++ username ++ " " ++ msg
  set ce [text := ""]
  writeChan chan $ Chat $ UserMessage username msg


showMsg :: TextCtrl () -> ChatMsg -> IO ()
showMsg ct = \case
  Say (UserHandle username' _) _ msg -> formatMsg ct blue username' msg
  Tell (UserHandle username' _) msg -> formatMsg ct blue username' msg
  UserMessage _ msg -> formatMsg ct black "Me" msg
  _ -> return ()


formatMsg :: TextCtrl () -> Color -> String -> String -> IO ()
formatMsg ct color speaker msg = do
  setTextAttr ct 12 color
  appendText ct $ speaker ++ ": " ++ msg ++ "\n"


setTextAttr :: TextCtrl () -> Int -> Color -> IO ()
setTextAttr ct fsize color = do
  (font', _) <- fontCreateFromStyle fontFixed {_fontSize = fsize}
  attr <- textAttrCreate color white font'
  void $ textCtrlSetDefaultStyle ct attr
