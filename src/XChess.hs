module Main where

import Lentils.Fics.FicsConnection
import Lentils.Wx.ToolBox

import Graphics.UI.WX

-- TODO: add game result to PGN
-- TODO: close eventLoop after GameResult
-- TODO: add initial move to vMoves if not empty!
-- TODO: application icon

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
