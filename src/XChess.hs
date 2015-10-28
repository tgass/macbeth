module Main where

import Control.Concurrent.Chan
import Lentils.Fics.FicsConnection (ficsConnection)
import Lentils.Wx.WxLogin
import Control.Monad
import Graphics.UI.WX

--TODO: fix warnings
--TODO: Rename packages. Wx.WxLogin -> ./wx/WxLogin
--TODO: cleanup ghc, modules: ghc-pkg list -v, ghc-pkg dot
main :: IO ()
main = do
  (h, chan) <- ficsConnection
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
