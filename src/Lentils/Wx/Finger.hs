module Lentils.Wx.Finger (
  wxFinger
) where

import Control.Monad
import Graphics.UI.WX
import Graphics.UI.WXCore


wxFinger :: String -> String -> IO ()
wxFinger name stats = do
  f <- frameFixed [ text := "Finger of " ++ name]
  st <- staticText f [ text := "Finger of " ++ name ++ stats
                     , font := fontFixed
                     , fontSize := 14]
  set f [layout := margin 10 $ row 0 [widget st]]
  void $ windowShow f
