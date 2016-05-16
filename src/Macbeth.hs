module Main where

import Macbeth.Fics.FicsConnection
import Macbeth.Wx.ToolBox
import Macbeth.Wx.Sounds

import Graphics.UI.WX
import Sound.ALUT


main :: IO ()
main = withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
  sounds <- initSound
  (h, chan) <- ficsConnection
  start $ wxToolBox h chan sounds
