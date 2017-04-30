module Macbeth.Wx.Finger (
  wxInfo
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Player
import Macbeth.Wx.Utils

import Control.Monad.Cont
import Control.Concurrent.Chan
import Graphics.UI.WX


wxInfo :: FicsMessage -> Chan FicsMessage -> IO ()
wxInfo msg chan = runCont (basicFrame (frameConfig msg) chan) $ setupFrame msg


frameConfig :: FicsMessage -> FrameConfig
frameConfig msg = FrameConfig {
  fCreate = frameFixed,
  fTitle = title msg,
  hasStatusField = False
}


setupFrame :: FicsMessage -> (Panel (), StatusField, FrameActions) -> IO ()
setupFrame msg (p, _, _) = do
  st <- staticText p [ text := showAll msg
                     , font := fontFixed
                     , fontSize := 14]
  set p [layout := margin 10 $ row 0 [widget st]]


title :: FicsMessage -> String
title (Finger userHandle _) = "Finger of " ++ name userHandle
title (History userHandle _) = "History for " ++ name userHandle
title _ = ""


showAll :: FicsMessage -> String
showAll m@(Finger _ msg) = title m ++ msg
showAll m@(History _ msg) = title m ++ "\n" ++ msg
showAll _ = ""
