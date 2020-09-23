module Macbeth.Wx.Dialog where

import           Graphics.UI.WX


quit :: Frame () -> IO (Maybe Int)
quit f = do
  d <- dialog f [text := "Quitting Macbeth..."]
  p <- panel d []
  b_ok  <- button p [text := "Quit"]
  b_can  <- button p [text := "Cancel"]

  set d [ defaultButton := b_ok
        , layout := container p $ margin 10 $ column 5 [
              boxed "" (margin 10 $ label "Do you really want to quit Macbeth?")
            , floatBottomRight $ row 5 [widget b_can, widget b_ok]]
        ]

  showModal d $ \stop -> do
    set b_ok [on command := stop $ Just 1]
    set b_can [on command := stop Nothing]

