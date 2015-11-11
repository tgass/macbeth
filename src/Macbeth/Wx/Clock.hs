module Macbeth.Wx.Clock (
  ChessClock (..),
  Clock (..),
  newClock,
  updateChessClock,
  stopChessClock
) where

import Macbeth.Api.Api
import Macbeth.Api.Move
import Macbeth.Utils.Utils
import Macbeth.Wx.Utils

import Control.Concurrent.Chan ()
import Control.Concurrent.STM
import Graphics.UI.WX hiding (when, position)


data Clock = Clock {color :: PColor, time :: TVar Int, timerCtrl :: Timer, clockTxt :: StaticText () }

data ChessClock = ChessClock {clockW :: Clock, clockB :: Clock}


stopChessClock cc =
  set (timerCtrl $ clockW cc) [enabled := False] >>
  set (timerCtrl $ clockB cc) [enabled := False]


updateChessClock :: Move -> ChessClock -> IO ()
updateChessClock m cc = updateClock m White (clockW cc) >> updateClock m Black (clockB cc)


updateClock :: Move -> PColor -> Clock -> IO ()
updateClock move color clock = do
  let time' = remainingTime color move
  atomically $ swapTVar (time clock) time'
  set (clockTxt clock) [text := formatTime time']
  set (timerCtrl clock) [enabled := isActive color move]

isActive :: PColor -> Move -> Bool
isActive c m = if moveNumber m == 1 then False else turn m == c


newClock :: Panel () -> PColor -> Move -> IO Clock
newClock p c m = do
  t <- newTVarIO (remainingTime c m)
  st <- staticTextFormatted p (formatTime $ remainingTime c m)
  tx <- timer p [ interval := 1000, on command := updateTime t st ]
  return $ Clock c t tx st


updateTime :: TVar Int -> StaticText () -> IO ()
updateTime vTime st = do
  time <- atomically $ modifyTVar vTime (\t -> t - 1) >> readTVar vTime
  set st [text := formatTime time]

