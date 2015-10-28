module Lentils.Wx.WxUtils (
  eventLoop,
  toWxColor
) where

import Lentils.Api.Api
import Lentils.Api.CommandMsg

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Graphics.UI.WXCore

eventLoop :: Int -> Chan CommandMsg -> MVar CommandMsg -> Frame () -> IO ()
eventLoop id chan vCmd f = readChan chan >>= putMVar vCmd >>
  commandEventCreate wxEVT_COMMAND_MENU_SELECTED id >>= evtHandlerAddPendingEvent f >>
  eventLoop id chan vCmd f

toWxColor :: Lentils.Api.Api.PColor -> Graphics.UI.WXCore.Color
toWxColor White = white
toWxColor Black = black
