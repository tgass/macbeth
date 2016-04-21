{-# LANGUAGE DeriveGeneric #-}

module Macbeth.Wx.Configuration (
  wxConfiguration
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Wx.Utils

import Control.Concurrent.Chan
import Control.Monad.Except
import GHC.Generics
import Graphics.UI.WX
import Graphics.UI.WXCore
import qualified Macbeth.Fics.Configuration as C

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y


data ConfigSubset = ConfigSubset {
    directory :: String
  , autologin :: Bool
} deriving (Show, Generic)


instance Y.FromJSON ConfigSubset
instance Y.ToJSON ConfigSubset


wxConfiguration :: Chan FicsMessage -> IO ()
wxConfiguration chan = do
  f  <- frame [ text := "Macbeth"]
  ct <- textCtrlEx f (wxTE_MULTILINE .+. wxTE_RICH) [font := fontFixed]
  showConfig ct
  status <- statusField []

  b_current <- button f [text := "Reset", on command := showConfig ct]
  b_save <- button f [ text := "Save",
                       on command := either (\_ -> set status [text := "Illegal file format."]) return
                                     =<< runExceptT (parseAndSave ct status)]


  set f [ statusBar := [status],
          layout := margin 10 $ column 10 [
                       boxed "Configuration" $ fill $ minsize (Size 380 220) $ widget ct
                     , hfloatRight $ row 5 [widget b_current, widget b_save]]
        ]
  registerWxCloseEventListener f chan


showConfig :: TextCtrl() -> IO ()
showConfig ct = do
  config <- C.loadConfig
  set ct [text := comments ++ BS.unpack (Y.encode $ toSubset config)]


parseAndSave :: TextCtrl () -> StatusField -> ExceptT Y.ParseException IO ()
parseAndSave ct status = do
  config <- liftIO C.loadConfig
  sub <- ExceptT $ (Y.decodeEither' . BS.pack) `fmap` get ct text
  liftIO $ do
    C.saveConfig (fromSubset config sub)
    set status [text := "Configuration saved."]


toSubset :: C.Config -> ConfigSubset
toSubset c = ConfigSubset (C.directory c) (C.autologin c)


fromSubset :: C.Config -> ConfigSubset -> C.Config
fromSubset config sub = config { C.directory = directory sub, C.autologin = autologin sub}


comments :: String
comments = "# Changes will take effect only after restart (for now).\n\n\
           \# If you change the directory make sure it exists.\n\
           \# It will not be created for you.\n\n"
