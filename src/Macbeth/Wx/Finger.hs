module Macbeth.Wx.Finger (
  wxFinger
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Wx.Utils

import Control.Concurrent.Chan
import Graphics.UI.WX

wxFinger :: String -> String -> Chan FicsMessage -> IO ()
wxFinger name stats chan = do
  f <- frameFixed [ text := "Finger of " ++ name]
  st <- staticText f [ text := "Finger of " ++ name ++ stats
                     , font := fontFixed
                     , fontSize := 14]
  set f [layout := margin 10 $ row 0 [widget st]]
  registerWxCloseEventListener f chan
