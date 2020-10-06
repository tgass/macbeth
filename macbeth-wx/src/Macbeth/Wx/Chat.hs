module Macbeth.Wx.Chat (
    wxChat
  , Receiving(..)
) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Monoid
import           Graphics.UI.WX hiding (when, next)
import           Graphics.UI.WXCore hiding (when)
import           Macbeth.Fics.Message hiding (gameId)
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Player hiding (handle)
import qualified Macbeth.Fics.Api.Result as R
import qualified Macbeth.Wx.Commands as Cmds
import qualified Macbeth.Wx.Config.UserConfig as C
import           Macbeth.Wx.Utils
import           Macbeth.Wx.RuntimeEnv (RuntimeEnv)
import qualified Macbeth.Wx.RuntimeEnv as E
import           System.IO

eventId :: Int
eventId = wxID_HIGHEST + 1

data Receiving = Receiving String String deriving (Show)

data SpeakMode = TellMode | SayMode | WhisperMode | KibitzMode deriving (Eq, Enum, Bounded)

wxChat :: RuntimeEnv -> ChatId -> Maybe Receiving -> Chan Message -> IO ()
wxChat env chatId mMsg chan = do
  speakModes <- initSpeakModes env chatId
  f <- frame [ text := show chatId ]
  windowSetFocus f
  p <- panel f []
 
  modeVar <- newTVarIO speakModes

  ct <- textCtrlEx p (wxTE_MULTILINE .+. wxTE_RICH .+. wxTE_READONLY) [font := fontFixed {_fontSize = env `E.getConfig` C.fontSize}]
  maybe (return ()) (showReceiving ct) mMsg

  ce <- entry p []
  set ce [on enterKey := message ce ct modeVar chatId (E.handle env) chan ]

  status <- statusField [ text := show $ head $ speakModes ]

  set f [ statusBar := [status]
        , layout := minsize (Size 400 200) $ container p $ margin 10 $
                      column 5 [ fill $ widget ct, hfill $ widget ce]]

  windowOnKeyDown ct $ \evt -> if
    | keyWithMod evt 'W' justControl -> close f
    | keyWithMod evt 'M' justControl -> toggleSpeakMode status modeVar
    | otherwise -> propagateEvent

  windowOnKeyDown ce $ \evt -> if
    | keyWithMod evt 'W' justControl -> close f
    | keyWithMod evt 'M' justControl -> toggleSpeakMode status modeVar
    | otherwise -> propagateEvent
  
  windowOnKeyDown p $ \evt -> if
    | keyWithMod evt 'W' justControl -> close f
    | keyWithMod evt 'M' justControl -> toggleSpeakMode status modeVar
    | otherwise -> return ()

  threadId <- eventLoop f eventId chan $ \case

    Tells (UserHandle username _) Nothing msg -> when (UserChat username == chatId) $ showReceiving ct $ Receiving username msg

    Tells (UserHandle username _) (Just channelId) msg -> when (ChannelChat channelId == chatId) $ showReceiving ct $ Receiving username msg

    Says (UserHandle username _) (Just gameId) msg -> when (GameChat gameId == chatId) $ showReceiving ct $ Receiving username msg

    Whispers (UserHandle username _) rating gameId msg -> when (GameChat gameId == chatId) $ showReceiving ct $ Receiving username msg

    Kibitzes (UserHandle username _) rating gameId msg -> when (GameChat gameId == chatId) $ showReceiving ct $ Receiving username msg

    GameResult result -> when (GameChat (R.gameId result) == chatId) $ showSystemMsg ct $ "--- Game " <> show (R.gameId result) <> " has ended. ---"


    IllegalWhisper Nothing -> showSystemMsg ct $ "You are not playing or observing a game."

    IllegalWhisper (Just gameId) -> when (GameChat gameId == chatId) $ showSystemMsg ct $ "You are not observing game " <> show gameId <> "."

    WxClose -> close f

    _ -> return ()

  windowOnDestroy f $ do
    env `E.untrackChat` chatId
    killThread threadId


message :: TextCtrl () -> TextCtrl () -> TVar [SpeakMode] -> ChatId -> Handle -> Chan Message -> IO ()
message ce ct modeVar chatId h chan = do
  mode <- head <$> readTVarIO modeVar
  msg <- get ce text
  runCommand chatId mode h msg
  set ce [text := ""]
  showMessage ct black (Just "Me") msg


runCommand :: ChatId -> SpeakMode -> Handle -> String -> IO ()
runCommand (ChannelChat cid) _ h msg = Cmds.tellChannel h cid msg
runCommand (UserChat username) _ h msg = Cmds.tell h username msg
runCommand (GameChat _) SayMode h msg = Cmds.say h msg
runCommand (GameChat gameId) WhisperMode h msg = Cmds.whisper h gameId msg
runCommand (GameChat gameId) KibitzMode h msg = Cmds.kibitz h gameId msg
runCommand _ _ _ _= return ()


showReceiving :: TextCtrl () -> Receiving -> IO ()
showReceiving ct (Receiving username msg) = showMessage ct blue (Just username) msg


showSystemMsg :: TextCtrl () -> String -> IO ()
showSystemMsg ct msg = showMessage ct red Nothing msg


showMessage :: TextCtrl () -> Color -> Maybe String -> String -> IO ()
showMessage ct color mSpeaker msg = do
  (font', _) <- fontCreateFromStyle fontFixed {_fontSize = 12}
  attr <- textAttrCreate color white font'
  void $ textCtrlSetDefaultStyle ct attr
  appendText ct $ (maybe "" (++ ": ") mSpeaker) ++ msg ++ "\n"


toggleSpeakMode :: StatusField -> TVar [SpeakMode] -> IO ()
toggleSpeakMode st modeVar = do
  next <- atomically $ do
    modifyTVar modeVar $ drop 1
    head <$> readTVar modeVar
  set st [text := show next ]


initSpeakModes :: RuntimeEnv -> ChatId -> IO [SpeakMode]
initSpeakModes _ (ChannelChat _) = return $ repeat TellMode
initSpeakModes _ (UserChat _) = return $ repeat TellMode
initSpeakModes env (GameChat gameId) = do
  isPlaying <- env `E.isPlaying` gameId
  if isPlaying 
    then return $ cycle [SayMode, WhisperMode, KibitzMode]
    else return $ cycle [WhisperMode, KibitzMode]


instance Show SpeakMode where
  show TellMode = "Tell"
  show SayMode = "Say"
  show WhisperMode = "Whisper"
  show KibitzMode = "Kibitz"

