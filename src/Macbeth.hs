module Main where

import Macbeth.Fics.FicsConnection
import Macbeth.Wx.ToolBox
import Macbeth.Wx.RuntimeEnv

import Graphics.UI.WX
import Sound.ALUT


main :: IO ()
main = withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
  (h, chan) <- ficsConnection
  env <- initRuntime h
  start $ wxToolBox env chan
