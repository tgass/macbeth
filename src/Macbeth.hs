module Main where

import Macbeth.Fics.FicsConnection
import Macbeth.Wx.ToolBox

import Graphics.UI.WX

-- TODO: application icon
-- TODO: Cmd+W, Cmd+Q
-- while observing: TextMessage "Game 84: Paet offers a draw."
-- unobserve does not work?
-- paint board background with Rects

-- ARCHITEX
-- TODO: close all windows if ToolBox closes
-- newtype for perspective == view
-- TODO: user frameCreateTopFrame for background / hibernation /network conn evnt
-- TODO: provide more logging choice
-- TODO: create new frames with complete channel, WxNewFrame CommandMsg (Chan CommandMsg)
-- declinedChallenge / offerDeclined


-- BUGS
-- TODO: Seek auto match ?! Bug?

-- FEATURES
-- command moves to get first moves in observed game?

-- MINOR


main :: IO ()
main = do
  (h, chan) <- ficsConnection
  start $ wxToolBox h chan
