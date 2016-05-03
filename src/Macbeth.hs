module Main where

import Macbeth.Fics.FicsConnection
import Macbeth.OSX.Sounds
import Macbeth.Wx.ToolBox

import Control.Concurrent
import Graphics.UI.WX

main :: IO ()
main = do
  (h, chan) <- ficsConnection
  forkIO $ sounds =<< dupChan chan
  start $ wxToolBox h chan
