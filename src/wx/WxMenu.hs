module WxMenu (
  wxMenu
) where

import WxSeek
import WxMatch

import Control.Monad (liftM)
import Graphics.UI.WX
import Graphics.UI.WXCore (windowShow)
import System.IO (Handle, hPutStrLn)


wxMenu :: Handle -> IO (Menu())
wxMenu h = do
  actions <- menuPane [text := "Actions"]
  match <- menuItem actions [text := "Match", on command := wxMatch h]
  seek <- menuItem actions [text := "Seek", on command := (wxSeek h) ]
  return actions
