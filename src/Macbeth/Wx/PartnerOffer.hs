module Macbeth.Wx.PartnerOffer (
  wxPartnerOffer
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Player
import Macbeth.Wx.Utils

import Control.Concurrent
import Graphics.UI.WX
import System.IO


wxPartnerOffer :: Handle -> UserHandle -> Chan FicsMessage  -> IO ()
wxPartnerOffer h userHandle chan = do
  f <- frame []
  p <- panel f []

  b_accept  <- button p [text := "Accept", on command := hPutStrLn h ("5 partner" ++ name userHandle) >> close f]
  b_decline <- button p [text := "Decline", on command := hPutStrLn h "5 decline" >> close f]
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
