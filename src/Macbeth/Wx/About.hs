module Macbeth.Wx.About (
  wxAbout
) where

import Paths

import Graphics.UI.WX
import System.IO.Unsafe


wxAbout :: IO ()
wxAbout = do
  f <- frameFixed [ text := "About Macbeth, Free FICS client "]
  p <- panel f [ on paint := paintAbout]
  set f [ layout := fill $ minsize (Size 300 148) $ widget p]
  return ()

paintAbout :: DC a -> t -> IO ()
paintAbout dc _ = drawBitmap dc (bitmap $ unsafePerformIO $ getDataFileName "about.jpg") (point 0 0) False []

