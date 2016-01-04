module Macbeth.Wx.Configuration (
  wxConfiguration
) where

import Macbeth.Fics.Configuration
import Macbeth.Fics.FicsMessage
import Macbeth.Wx.Utils

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Except
import Graphics.UI.WX
import Graphics.UI.WXCore

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y

eventId = wxID_HIGHEST + 61

wxConfiguration :: Chan FicsMessage -> IO ()
wxConfiguration chan = do
  f  <- frame [ text := "Macbeth"]
  ct <- textCtrlEx f (wxTE_MULTILINE .+. wxTE_RICH) [font := fontFixed]
  showConfig ct loadConfig
  status <- statusField []

  b_default <- button f [text := "Default", on command := showConfig ct loadDefaultConfig]
  b_current <- button f [text := "Current", on command := showConfig ct loadConfig]
  b_save <- button f [ text := "Save",
                       on command := void $ runExceptT $ parseAndSave ct status `catchError` showError status]


  set f [ statusBar := [status],
          layout := margin 10 $ column 10 [
                       boxed "Configuration" $ fill $ minsize (Size 380 220) $ widget ct
                     , hfloatRight $ row 5 [widget b_default, widget b_current, widget b_save]]
        ]
  registerWxCloseEventListener chan eventId f


showConfig :: TextCtrl() -> IO Config -> IO ()
showConfig ct io_config = io_config >>= \config -> set ct [text := comments ++ BS.unpack (Y.encode config)]


parseAndSave :: TextCtrl () -> StatusField -> ExceptT Y.ParseException IO ()
parseAndSave ct status = do
  config <- ExceptT $ (Y.decodeEither' . BS.pack) `fmap` get ct text
  liftIO $ saveConfig config
  liftIO $ set status [text := "Configuration saved."]


showError :: StatusField -> Y.ParseException -> ExceptT Y.ParseException IO ()
showError status _ = liftIO $ set status [text := "Illegal file format."]


comments :: String
comments = "# Changes will take effect only after restart (for now).\n\n\
           \# If you change the directory make sure it exists.\n\
           \# It will not be created for you.\n\n"

