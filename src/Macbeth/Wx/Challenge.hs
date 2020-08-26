{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.Challenge (
  wxChallenge
) where

import           Control.Concurrent
import           Graphics.UI.WX
import           Graphics.UI.WXCore
import qualified Macbeth.Fics.Commands as Cmds
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Game
import           Macbeth.Wx.Utils
import           System.IO


wxChallenge :: Handle -> Challenge -> Chan Message  -> IO ()
wxChallenge h c chan = do
  vCmd <- newEmptyMVar

  f <- frame []
  p <- panel f []

  b_accept  <- button p [text := "Accept", on command := Cmds.accept h >> close f]
  b_decline <- button p [text := "Decline", on command := Cmds.decline h >> close f]
  st_params <- staticText p [ text := showChallenge c
                            , fontFace := "Avenir Next Medium"
                            , fontSize := 16
                            , fontWeight := WeightBold]

  set f [ defaultButton := b_accept
        , layout := container p $ margin 10 $
            column 5 [boxed "You received a challenge." (
              grid 5 5 [
                [ hfill $ widget st_params]]
            )
            , floatBottomRight $ row 5 [widget b_accept, widget b_decline]]
        ]

  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \case

      MatchRequested c' -> when (isUpdate c c') $ close f

      WxClose -> close f

      _ -> return ()

  threadId <- forkIO $ eventLoop eventId chan vCmd f
  windowOnDestroy f $ killThread threadId



eventId :: Int
eventId = wxID_HIGHEST + 1
