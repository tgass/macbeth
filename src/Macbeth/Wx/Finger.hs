module Macbeth.Wx.Finger (
  wxInfo
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Player
import Macbeth.Wx.Utils

import Control.Concurrent.Chan
import Graphics.UI.WX

wxInfo :: FicsMessage -> Chan FicsMessage -> IO ()
wxInfo msg chan = do
  f <- frameFixed [ text := title msg]
  st <- staticText f [ text := showAll msg
                     , font := fontFixed
                     , fontSize := 14]
  set f [layout := margin 10 $ row 0 [widget st]]
  registerWxCloseEventListener f chan

title :: FicsMessage -> String
title (Finger userHandle _) = "Finger of " ++ name userHandle
title (History userHandle _) = "History for " ++ name userHandle
title _ = ""


showAll :: FicsMessage -> String
showAll m@(Finger _ msg) = title m ++ msg
showAll m@(History _ msg) = title m ++ "\n" ++ msg
showAll _ = ""
