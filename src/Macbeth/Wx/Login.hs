module Macbeth.Wx.Login (
  wxLogin
) where

import Macbeth.Api.CommandMsg
import Macbeth.Wx.Utils

import Control.Applicative
import Control.Concurrent.Chan
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO

eventId = wxID_HIGHEST + 60

data WxLogin = WxLogin {
  name :: TextCtrl (),
  password :: TextCtrl ()
}


wxLogin :: Handle -> Chan CommandMsg -> IO ()
wxLogin h chan = do
  f <- frameFixed [ text := "FICS Login" ]
  p <- panel f []
  wxInputs <- loginInputs p

  status <- statusField []

  b_ok  <- button p [text := "Login", on command := okBtnHandler wxInputs f h chan]
  b_can <- button p [text := "Quit", on command := close f]

  set f [ defaultButton := b_ok
        , layout := container p $ margin 10 $ column 25 [
              boxed "" (grid 15 15 [
                [ label "Username:", hfill $ widget $ name wxInputs]
               ,[ label "Password:", hfill $ widget $ password wxInputs]])
            , floatBottomRight $ row 5 [widget b_ok, widget b_can]]
        , statusBar := [ status ]
        ]
  wxChan <- dupChan chan
  registerWxCloseEventListener wxChan eventId f


okBtnHandler :: WxLogin -> Frame() -> Handle -> Chan CommandMsg -> IO ()
okBtnHandler wxInputs f h chan = get (name wxInputs) text >>= hPutStrLn h >> loop >> close f
  where
    loop = readChan chan >>= handlePw

    handlePw :: CommandMsg -> IO ()
    handlePw Password = get (password wxInputs) text >>= hPutStrLn h
    handlePw (GuestLogin _) = hPutStrLn h ""
    handlePw _ = loop


loginInputs :: Panel () -> IO WxLogin
loginInputs p = WxLogin
  <$> textEntry p [ text := "guest", alignment := AlignRight]
  <*> textCtrlEx p wxTE_PASSWORD [ alignment := AlignRight]


