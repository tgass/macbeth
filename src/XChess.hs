module Main where

import Lentils.Fics.FicsConnection
import Lentils.Wx.ToolBox

import Graphics.UI.WX

-- TODO: application icon

-- ARCHITEX
-- TODO: close all windows if ToolBox closes
-- TODO: user frameCreateTopFrame for background / hibernation /network conn evnt
-- TODO: provide more logging choice
-- TODO: create new frames with complete channel, WxNewFrame CommandMsg (Chan CommandMsg)

-- BUGS
-- TODO: Seek auto match ?! Bug?

-- FEATURES
-- TODO: Pre-Moves
-- TODO: takeback

-- MINOR


main :: IO ()
main = do
  (h, chan) <- ficsConnection
  start $ wxToolBox h chan
