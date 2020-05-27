module Main where

import Control.Concurrent.Chan
import Graphics.UI.WX
import qualified Macbeth.Fics.Api.Game as G
import Macbeth.Wx.RuntimeEnv
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Rating
import Macbeth.Wx.Game.Game
import Sound.ALUT
import System.IO

main :: IO ()
main = withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
  env <- initRuntime stdout
  chan <- newChan
  start $ wxGame env (GameId 1) defaultGameParams chan
  
defaultGameParams :: G.GameParams
defaultGameParams = G.GameParams True "foo" Unrated "bar" Unrated True "hmm" 60 1
