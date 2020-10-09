module Macbeth.Wx.PartnerOffer (
  wxPartnerOffer
) where

import           Control.Concurrent
import           Graphics.UI.WX
import           Macbeth.Fics.Api.Player
import qualified Macbeth.Fics.Commands as Cmds
import           Macbeth.Fics.Message
import           Macbeth.Wx.Utils
import           Macbeth.Wx.RuntimeEnv


wxPartnerOffer :: RuntimeEnv -> UserHandle -> Chan Message  -> IO ()
wxPartnerOffer env userHandle chan = do
  f <- frame []
  p <- panel f []

  b_accept  <- button p [text := "Accept", on command := Cmds.accept env >> close f]
  b_decline <- button p [text := "Decline", on command := Cmds.decline env >> close f]
  st_params <- staticText p [ text := name userHandle ++ " offers to be your bughouse partner."
                            , fontFace := "Avenir Next Medium"
                            , fontSize := 16
                            , fontWeight := WeightBold]

  set f [ defaultButton := b_accept
        , layout := container p $ margin 10 $
            column 5 [boxed "Bughouse" (
              grid 5 5 [
                [ hfill $ widget st_params]]
            )
            , floatBottomRight $ row 5 [widget b_accept, widget b_decline]]
        ]

  dupChan chan >>= registerWxCloseEventListener f
