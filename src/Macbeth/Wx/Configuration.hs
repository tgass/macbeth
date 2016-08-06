module Macbeth.Wx.Configuration (
  wxConfiguration
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Wx.Utils
import Macbeth.Wx.Config.DefaultSounds
import qualified Macbeth.Wx.Config.UserConfig as C

import Control.Concurrent.Chan
import Control.Monad.Except
import Graphics.UI.WX hiding (fontSize)
import Graphics.UI.WXCore

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y


wxConfiguration :: Chan FicsMessage -> IO ()
wxConfiguration chan = do
  f  <- frame [ text := "Macbeth"]
  ct <- textCtrlEx f (wxTE_MULTILINE .+. wxTE_RICH) [font := fontFixed]
  showConfig ct
  status <- statusField []

  b_current <- button f [text := "Reset", on command := showConfig ct]
  b_resetSounds <- button f [text := "Reset Sounds", on command := resetSounds ct]
  b_save <- button f [ text := "Save",
                       on command := either (\_ -> set status [text := "Illegal file format."]) return
                                     =<< runExceptT (parseAndSave ct status)]


  set f [ statusBar := [status],
          layout := margin 10 $ column 10 [
                       boxed "Configuration" $ fill $ minsize (Size 380 220) $ widget ct
                     , hfloatRight $ row 5 [widget b_current, widget b_resetSounds, widget b_save]]
        ]
  registerWxCloseEventListener f chan


showConfig :: TextCtrl() -> IO ()
showConfig ct = do
  config <- C.loadConfig
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
