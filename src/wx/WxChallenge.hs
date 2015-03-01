module WxChallenge (
  wxChallenge
) where

import System.IO (Handle, hPutStrLn)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Graphics.UI.WX
import Graphics.UI.WXCore

import CommandMsg


wxChallenge :: Handle -> IO ()
wxChallenge h = do
  f <- frame []
  p <- panel f []

  b_accept  <- button p [text := "Accept", on command := close f ]
  b_decline <- button p [text := "Decline", on command := close f]
  b_adjourn <- button p [text := "Adjourn", on command := close f]

  set f [ defaultButton := b_accept
        , layout := container p $ margin 10 $
            column 5 [boxed "You have been challenged." (
              floatBottomRight $ row 5 [widget b_accept, widget b_decline]
            )]
        ]

  windowShow f
  return ()


