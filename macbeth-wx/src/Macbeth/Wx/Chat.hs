module Macbeth.Wx.Chat where

import           Control.Concurrent
import           Control.Monad
import           Graphics.UI.WX hiding (when)
import           Graphics.UI.WXCore hiding (when)
import           Macbeth.Fics.Message hiding (gameId)
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Chat
import           Macbeth.Fics.Api.Player hiding (handle)
import qualified Macbeth.Wx.Commands as Cmds
import qualified Macbeth.Wx.Config.UserConfig as C
import           Macbeth.Wx.Utils
import qualified Macbeth.Wx.RuntimeEnv as E
import           System.IO

eventId :: Int
eventId = wxID_HIGHEST + 1

wxChat :: E.RuntimeEnv -> ChatId -> [ChatMessage] -> Chan Message -> IO ()
wxChat env chatId msgs chan = do
  f <- frame [ text := show chatId ]
  windowSetFocus f
  p <- panel f []

  ct <- textCtrlEx p (wxTE_MULTILINE .+. wxTE_RICH .+. wxTE_READONLY) [font := fontFixed {_fontSize = env `E.getConfig` C.fontSize}]
  prefill ct msgs

  ce <- entry p []
  set ce [on enterKey := tell ce chatId (E.handle env) chan]

  set f [layout := minsize (Size 400 200) $ container p $ margin 10 $
                     column 5 [ fill $ widget ct, hfill $ widget ce]]

  windowOnKeyDown p (\evt -> if
    | keyWithMod evt 'W' justControl -> close f
    | otherwise -> return ())

  threadId <- eventLoop f eventId chan $ \case

    Tells (UserHandle username _) Nothing msg -> when (UserChat username == chatId) $ showMsg ct $ From username msg

    Tells (UserHandle username _) (Just channelId) msg -> when (ChannelChat channelId == chatId) $ showMsg ct $ From username msg

    Says (UserHandle username _) (Just gameId) msg -> when (GameChat gameId == chatId) $ showMsg ct $ From username msg

    WxClose -> close f

    _ -> return ()

  windowOnDestroy f $ do
    writeChan chan $ CloseChat chatId
    killThread threadId


prefill :: TextCtrl () -> [ChatMessage] -> IO ()
prefill _ [] = return ()
prefill ct (msg:mx) = showMsg ct msg >> prefill ct mx

tell :: TextCtrl () -> ChatId -> Handle -> Chan Message -> IO ()
tell ce chatId h chan = do
  msg <- get ce text
  runCommand h chatId msg
  set ce [text := ""]
  writeChan chan $ UserMessage chatId msg

runCommand :: Handle -> ChatId -> String -> IO ()
runCommand h (ChannelChat cid) msg = Cmds.tellChannel h cid msg
runCommand h (UserChat username) msg = Cmds.tell h username msg
runCommand h (GameChat _) msg = Cmds.say h msg

showMsg :: TextCtrl () -> ChatMessage -> IO ()
showMsg ct (From username msg) = formatMsg ct blue username msg
showMsg ct (Self msg) = formatMsg ct black "Me" msg


formatMsg :: TextCtrl () -> Color -> String -> String -> IO ()
formatMsg ct color' speaker msg = do
  setTextAttr ct 12 color'
  appendText ct $ speaker ++ ": " ++ msg ++ "\n"


setTextAttr :: TextCtrl () -> Int -> Color -> IO ()
setTextAttr ct fsize color' = do
  (font', _) <- fontCreateFromStyle fontFixed {_fontSize = fsize}
  attr <- textAttrCreate color' white font'
  void $ textCtrlSetDefaultStyle ct attr

