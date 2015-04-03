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
