module WxUtils (
  toWxColor
) where

import Api
import Graphics.UI.WXCore

toWxColor :: Api.Color -> Graphics.UI.WXCore.Color
toWxColor White = white
toWxColor Black = black
