module Main where

import Macbeth.Fics.FicsConnection
import Macbeth.OSX.Sounds
import Macbeth.Wx.ToolBox
import Macbeth.Wx.UserConfig hiding (sounds)

import Control.Concurrent
import Graphics.UI.WX

main :: IO ()
main = do
  (h, chan) <- ficsConnection
  start $ wxToolBox h chan
