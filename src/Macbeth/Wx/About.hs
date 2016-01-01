module Macbeth.Wx.About (
  wxAbout
) where

import Macbeth.Fics.Api.CommandMsg
import Macbeth.Wx.Utils
import Paths

import Control.Concurrent.Chan
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import System.IO.Unsafe

eventId = wxID_HIGHEST + 52

wxAbout :: Chan CommandMsg -> IO ()
wxAbout chan = do
  f <- frameFixed [ text := "About Macbeth, Free FICS client "]
  p <- panel f [ on paint := paintAbout]
  set f [ layout := fill $ minsize (Size 300 148) $ widget p]

  registerWxCloseEventListener chan eventId f

paintAbout :: DC a -> t -> IO ()
paintAbout dc _ = drawBitmap dc (bitmap $ unsafePerformIO $ getDataFileName "about.jpg") (point 0 0) False []
