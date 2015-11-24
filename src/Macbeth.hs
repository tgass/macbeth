module Main where

import Macbeth.Fics.FicsConnection
import Macbeth.Wx.ToolBox

import Graphics.UI.WX

-- TODO: application icon
-- command moves to get first moves in observed game?
-- newtype for perspective == view
-- declinedChallenge / offerDeclined
-- ESC deletes preMoves

-- ARCHITEX
-- TODO: close all windows if ToolBox closes
-- TODO: user frameCreateTopFrame for background / hibernation /network conn evnt
-- TODO: provide more logging choice
-- TODO: create new frames with complete channel, WxNewFrame CommandMsg (Chan CommandMsg)

-- BUGS
-- TODO: Seek auto match ?! Bug?

-- FEATURES
-- TODO: takeback

-- MINOR


main :: IO ()
main = do
  (h, chan) <- ficsConnection
  start $ wxToolBox h chan
