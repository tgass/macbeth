module Main where

import Macbeth.Fics.FicsConnection
import Macbeth.Wx.ToolBox

import Graphics.UI.WX

-- TODO: application icon
-- TODO: Cmd+W, Cmd+Q
-- unobserve does not work?
-- out of time msg did not show (Schoon vs..)
-- save game if observing optional
-- wild games
-- crazyhouse / bughouse
-- status bar with multiple fields!

-- ARCHITEX
-- TODO: close all windows if ToolBox closes
-- newtype for perspective == view
-- TODO: user frameCreateTopFrame for background / hibernation /network conn evnt
-- TODO: create new frames with complete channel, WxNewFrame CommandMsg (Chan CommandMsg)
-- declinedChallenge / offerDeclined
-- refactor FicsConnection (performance)


-- BUGS
-- TODO: Seek auto match ?! Bug?

-- FEATURES
-- command moves to get first moves in observed game?
-- while observing: TextMessage "Game 84: Paet offers a draw."

-- MINOR

main :: IO ()
main = do
  (h, chan) <- ficsConnection
  start $ wxToolBox h chan
