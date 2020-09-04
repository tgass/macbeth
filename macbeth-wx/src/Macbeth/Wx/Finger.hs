module Macbeth.Wx.Finger (
  wxInfo
) where

import Macbeth.Fics.Message
import Macbeth.Fics.Api.Player
import Macbeth.Wx.Utils

import Control.Monad.Cont
import Control.Concurrent.Chan
import Graphics.UI.WX


wxInfo :: Message -> Chan Message -> IO ()
wxInfo msg chan = runCont (basicFrame (frameConfig msg) chan) $ setupFrame msg


frameConfig :: Message -> FrameConfig
frameConfig msg = FrameConfig {
  fCreate = frameFixed,
  fTitle = title msg,
  hasStatusField = False
}


setupFrame :: Message -> (Panel (), StatusField, FrameActions) -> IO ()
setupFrame msg (p, _, _) = do
  st <- staticText p [ text := showAll msg
                     , font := fontFixed
                     , fontSize := 14]
  set p [layout := margin 10 $ row 0 [widget st]]


title :: Message -> String
title (Finger userHandle _) = "Finger of " ++ name userHandle
title (History userHandle _) = "History for " ++ name userHandle
title _ = ""


showAll :: Message -> String
showAll m@(Finger _ msg) = title m ++ msg
showAll m@(History _ msg) = title m ++ "\n" ++ msg
showAll _ = ""
