module Main where

import Lentils.Fics.FicsConnection
import Lentils.Wx.ToolBox

import Graphics.UI.WX

main :: IO ()
main = do
  (h, chan) <- ficsConnection
  start $ wxToolBox h chan
