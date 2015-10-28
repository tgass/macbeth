module Lentils.Wx.WxAbout (
  wxAbout
) where

import Graphics.UI.WX


wxAbout :: IO ()
wxAbout = do
  f <- frameFixed [ text := "About Lentil, Free FICS client "]
  p <- panel f [ on paint := paintAbout]
  set f [ layout := fill $ minsize (Size 300 148) $ widget p]
  return ()

paintAbout :: DC a -> t -> IO ()
paintAbout dc view = drawBitmap dc (bitmap "/Users/tilmann/Documents/leksah/XChess/gif/about.jpg") (point 0 0) False []

