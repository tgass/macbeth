module Main where

import Macbeth.Fics.FicsConnection
import Macbeth.Wx.ToolBox

import Graphics.UI.WX

main :: IO ()
main = do
  (h, chan) <- ficsConnection
  start $ wxToolBox h chan
