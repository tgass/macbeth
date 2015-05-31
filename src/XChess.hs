module Main where

import Control.Concurrent.Chan
import FicsConnection2 (ficsConnection)
import WxLogin

import Graphics.UI.WX

main :: IO ()
main = do
  chan <- newChan
  h <- ficsConnection $ \h cmd -> writeChan chan cmd
  start $ wxLogin h chan






--iv_lock
--
--  Setting ivariable lock stops the further changing of ivariables until the
--user logs out and in again. This is to stop users tampering with the ivariables
--and causing the interface to malfunction.
--
--See Also:  iset ivariables
--
--[Last modified: August 1st, 2000 -- DAV]
