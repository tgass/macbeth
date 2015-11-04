module Main where

import Lentils.Fics.FicsConnection
import Lentils.Wx.ToolBox

import Graphics.UI.WX

-- TODO: close eventLoop after GameResult
-- TODO: games list: filter private games
-- TODO: application icon

-- TODO: close all windows if ToolBox closes
-- TODO: user frameCreateTopFrame for background / hibernation /network conn evnt

-- TODO: Request move take backend, accept/ decline move takeback request
-- TODO: Save observed game with initial position
-- TODO: create new frames with complete channel, WxNewFrame CommandMsg (Chan CommandMsg)
-- TODO: make game list sortable, configurable
-- TODO: provide more logging choice

-- TODO: UX: show that challenge was an update
-- TODO: Seek auto match ?! Bug?

main :: IO ()
main = do
  (h, chan) <- ficsConnection
  start $ wxToolBox h chan
