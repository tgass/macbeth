module Lentils.Wx.Challenge (
  wxChallenge
) where

import Lentils.Api.Challenge
import Lentils.Api.CommandMsg
import Lentils.Wx.Utils (eventLoop)

import Control.Concurrent
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO

eventId = wxID_HIGHEST + 97

wxChallenge :: Handle -> Challenge -> Chan CommandMsg  -> IO ()
wxChallenge h c chan = do
  vCmd <- newEmptyMVar

  f <- frame []
  p <- panel f []

  b_accept  <- button p [text := "Accept", on command := hPutStrLn h "5 accept" >> close f]
  b_decline <- button p [text := "Decline", on command := hPutStrLn h "5 decline" >> close f]
  b_adjourn <- button p [text := "Adjourn", on command := hPutStrLn h "5 adjourn" >> close f]
  st_params <- staticText p [ text := displayChallenge c
                            , fontFace := "Avenir Next Medium"
                            , fontSize := 16
                            , fontWeight := WeightBold]

  set f [ defaultButton := b_accept
        , layout := container p $ margin 10 $
            column 5 [boxed "You received a challenge." (
              grid 5 5 [
                [ hfill $ widget st_params]]
            )
            , floatBottomRight $ row 5 [widget b_accept, widget b_decline, widget b_adjourn]]
        ]

  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd -> case cmd of

      MatchRequested c' -> when (isUpdate c c') $ close f

      _ -> return ()

  threadId <- forkIO $ eventLoop eventId chan vCmd f
  windowOnDestroy f $ killThread threadId >> hPutStrLn h "5 adjourn"


isUpdate :: Challenge -> Challenge -> Bool
isUpdate c c' = (nameW c == nameW c') && (nameB c == nameB c')


--main = start $ wxChallenge undefined (Challenge "foobar" (Rating 1200) "barbaz" Guest "12 2 blitz")
