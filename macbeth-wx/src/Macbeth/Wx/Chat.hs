module Macbeth.Wx.Chat (
  wxChat
) where

import           Control.Concurrent
import           Control.Monad
import           Control.Exception
import           Graphics.UI.WX hiding (when)
import           Graphics.UI.WXCore hiding (when)
import qualified Macbeth.Fics.Commands as Cmds
import           Macbeth.Fics.Message hiding (gameId)
import           Macbeth.Fics.Api.Chat
import           Macbeth.Fics.Api.Player hiding (handle)
import           Macbeth.Wx.Utils
import qualified Macbeth.Wx.RuntimeEnv as E
import qualified Macbeth.Wx.Config.UserConfig as C
import           System.IO

eventId :: Int
eventId = wxID_HIGHEST + 1


wxChat :: E.RuntimeEnv -> String -> a -> [ChatMsg] -> Chan Message -> IO ()
wxChat env chatPartner _ chatMsgs chan = do
  vCmd <- newEmptyMVar

  f <- frame [ text := "Chat with " ++ chatPartner]
  windowSetFocus f
  p <- panel f []

  ct <- textCtrlEx p (wxTE_MULTILINE .+. wxTE_RICH .+. wxTE_READONLY) [font := fontFixed {_fontSize = env `E.getConfig` C.fontSize}]
  prefill ct chatMsgs

  ce <- entry p []
  set ce [on enterKey := tell ce chatPartner (E.handle env) chan]

  set f [layout := minsize (Size 400 200) $ container p $ margin 10 $
                     column 5 [ fill $ widget ct, hfill $ widget ce]]

  windowOnKeyDown p (\evt -> if
    | keyWithMod evt 'W' justControl -> close f
    | otherwise -> return ())

  threadId <- forkIO $ eventLoop eventId chan vCmd f
  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \case

    Chat msg -> when (msg `belongsTo` chatPartner) $ showMsg ct msg

    WxClose -> close f

    _ -> return ()

  windowOnDestroy f $ do
    writeChan chan $ Chat $ CloseChat chatPartner
    sequence_ $ fmap (handle (\(_ :: IOException) -> return ()) . killThread) [threadId]


prefill :: TextCtrl () -> [ChatMsg] -> IO ()
prefill _ [] = return ()
prefill ct (m:mx) = showMsg ct m >> prefill ct mx


tell :: TextCtrl () -> Username -> Handle -> Chan Message -> IO ()
tell ce chatPartner h chan = do
  msg <- get ce text
  Cmds.tell h chatPartner msg
  set ce [text := ""]
  writeChan chan $ Chat $ UserMessage chatPartner msg


showMsg :: TextCtrl () -> ChatMsg -> IO ()
showMsg ct = \case
  Say (UserHandle chatPartner _) _ msg -> formatMsg ct blue chatPartner msg
  Tell (UserHandle chatPartner _) msg -> formatMsg ct blue chatPartner msg
  UserMessage _ msg -> formatMsg ct black "Me" msg
  _ -> return ()


formatMsg :: TextCtrl () -> Color -> String -> String -> IO ()
formatMsg ct color' speaker msg = do
  setTextAttr ct 12 color'
  appendText ct $ speaker ++ ": " ++ msg ++ "\n"


setTextAttr :: TextCtrl () -> Int -> Color -> IO ()
setTextAttr ct fsize color' = do
  (font', _) <- fontCreateFromStyle fontFixed {_fontSize = fsize}
  attr <- textAttrCreate color' white font'
  void $ textCtrlSetDefaultStyle ct attr
