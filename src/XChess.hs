module Main where

import FicsConnection2 (ficsConnection)
import Graphics.UI.WX
import WxToolBox
import Control.Concurrent.Chan


main :: IO ()
main = do
  chan <- newChan
  h <- ficsConnection $ writeChan chan
  start $ createToolBox h chan

