module Main where

import Lentils.Fics.FicsConnection
import Lentils.Wx.ToolBox

import Graphics.UI.WX

-- TODO: application icon

-- TODO: close all windows if ToolBox closes
-- TODO: user frameCreateTopFrame for background / hibernation /network conn evnt

-- TODO: Request move take backend, accept/ decline move takeback request
-- TODO: Save observed game with initial position
-- TODO: create new frames with complete channel, WxNewFrame CommandMsg (Chan CommandMsg)
-- TODO: make game list sortable, configurable, filter private games
-- TODO: provide more logging choice
-- TODO: Seek auto match ?! Bug?

main :: IO ()
main = do
  (h, chan) <- ficsConnection
  start $ wxToolBox h chan
