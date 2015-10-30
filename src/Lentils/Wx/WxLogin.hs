module Lentils.Wx.WxLogin (
  wxLogin
) where

import Lentils.Api.CommandMsg

import Control.Concurrent.Chan
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO (Handle, hPutStrLn)


wxLogin :: Handle -> Chan CommandMsg -> IO ()
wxLogin h chan = do

  f <- frameFixed [ text := "FICS Login" ]
  p <- panel f []
  e_name <- textEntry p [ text := "guest", alignment := AlignRight]
  e_pw   <- textCtrlEx p wxTE_PASSWORD [ alignment := AlignRight]

  status <- statusField []

  b_ok  <- button p [text := "Login", on command := okBtnHandler e_name e_pw f h chan]
  b_can <- button p [text := "Quit", on command := close f]

  set f [ defaultButton := b_ok
        , layout := container p $ margin 10 $
            column 25 [boxed "" (
              grid 15 15 [
                [ label "Username:", hfill $ widget e_name]
               ,[ label "Password:", hfill $ widget e_pw]]
            )
            , floatBottomRight $ row 5 [widget b_ok, widget b_can]]
        , statusBar := [ status ]
        ]


okBtnHandler :: TextCtrl() -> TextCtrl() -> Frame() -> Handle -> Chan CommandMsg -> IO ()
okBtnHandler e_name e_pw f h chan = do
  name <- get e_name text
  pw <- get e_pw text
  hPutStrLn h name
  loop pw
  where loop pw = readChan chan >>= handlePw pw
        handlePw pw' Password = hPutStrLn h pw' >> close f
        handlePw _ (GuestLogin _) = hPutStrLn h "" >> close f
        handlePw pw' _ = loop pw'

