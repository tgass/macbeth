module Macbeth.Wx.Configuration (
  wxConfiguration
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Wx.Config.Sounds
import Macbeth.Wx.RuntimeEnv
import Macbeth.Wx.Utils
import qualified Macbeth.Wx.Config.UserConfig as C

import Control.Monad.Cont
import Control.Concurrent.Chan
import Control.Monad.Except
import Graphics.UI.WX hiding (fontSize)
import Graphics.UI.WXCore

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y


wxConfiguration :: RuntimeEnv -> Chan FicsMessage -> IO ()
wxConfiguration env chan = runCont (basicFrame (frameConfig env) chan) setupFrame


frameConfig :: RuntimeEnv -> FrameConfig
frameConfig env = FrameConfig {
  fCreate = frame,
  fTitle = "Macbeth (" ++ getVersion env ++ ")",
  hasStatusField = True
}


setupFrame :: (Panel (), StatusField, FrameActions) -> IO ()
setupFrame (p, status, _) = do
  ct <- textCtrlEx p (wxTE_MULTILINE .+. wxTE_RICH) [font := fontFixed]
  showConfig ct

  b_current <- button p [text := "Reset", on command := showConfig ct]
  b_resetSounds <- button p [text := "Reset Sounds", on command := resetSounds ct]
  b_save <- button p [ text := "Save",
                       on command := either (\_ -> set status [text := "Illegal file format."]) return
                                     =<< runExceptT (parseAndSave ct status)]

  set p [ layout :=  margin 10 $ column 10 [
                       boxed "Configuration" $ fill $ minsize (Size 440 480) $ widget ct
                     , hfloatRight $ row 5 [widget b_current, widget b_resetSounds, widget b_save]]
        ]


showConfig :: TextCtrl() -> IO ()
showConfig ct = do
  config <-  C.loadConfig
  set ct [text := comments ++ BS.unpack (Y.encode $ removeUser config)]


resetSounds :: TextCtrl() -> IO ()
resetSounds ct = do
  c <- C.loadConfig
  set ct [text := comments ++ BS.unpack (Y.encode $ removeUser c{C.sounds = Just defaultSounds})]


parseAndSave :: TextCtrl () -> StatusField -> ExceptT Y.ParseException IO ()
parseAndSave ct status = do
  oldConf <- liftIO C.loadConfig
  newConf <- ExceptT $ (Y.decodeEither' . BS.pack) <$> get ct text
  liftIO $ do
    C.saveConfig (addUser newConf (C.user oldConf))
    set status [text := "Configuration saved."]


removeUser :: C.Config -> C.Config
removeUser c = c { C.user = Nothing }


addUser :: C.Config -> Maybe C.User -> C.Config
addUser c mUser = c { C.user = mUser }


comments :: String
comments = "# Changes will take effect only after restart (for now).\n\n\
           \# If you change the directory make sure it exists.\n\
           \# It will not be created for you.\n\n\
           \# You can use your own sounds by putting them in MACBETH_USER_DIR/sounds\n\n\n"
