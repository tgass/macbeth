module Main where

import Macbeth.Fics.FicsConnection
import Macbeth.Fics.Configuration
import Macbeth.Wx.ToolBox

import Graphics.UI.WX

main :: IO ()
main = do
  initConfig
  (h, chan) <- ficsConnection
  start $ wxToolBox h chan
