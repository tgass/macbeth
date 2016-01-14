module Macbeth.Wx.Login (
  wxLogin
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Wx.Utils

import Control.Applicative
import Control.Concurrent.Chan
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO

data WxLogin = WxLogin {
    name :: TextCtrl ()
  , password :: TextCtrl ()
  , guestLogin :: CheckBox ()
}


wxLogin :: Handle -> Chan FicsMessage -> IO ()
wxLogin h chan = do
  f <- frameFixed [ text := "Macbeth" ]
  p <- panel f []
  wxInputs <- loginInputs p
  set (guestLogin wxInputs) [on command := toggleLoginFields wxInputs]

  b_ok  <- button p [text := "Login", on command := okBtnHandler wxInputs f h chan]
  b_can <- button p [text := "Quit", on command := close f]

  set f [ defaultButton := b_ok
        , layout := container p $ margin 10 $ column 25 [
              boxed "Login @ freechess.org" (grid 15 15 [
                [ label "Username:", hfill $ widget $ name wxInputs]
               ,[ label "Password:", hfill $ widget $ password wxInputs]
               ,[ label "Login as Guest:", hfill $ widget $ guestLogin wxInputs]])
            , floatBottomRight $ row 5 [widget b_ok, widget b_can]]
        ]
  dupChan chan >>= registerWxCloseEventListener f


okBtnHandler :: WxLogin -> Frame() -> Handle -> Chan FicsMessage -> IO ()
okBtnHandler wxInputs f h chan = do
  isGuestLogin <- get (guestLogin wxInputs) checked
  username <- get (name wxInputs) text
  hPutStrLn h (if isGuestLogin then "guest" else username) >> loop >> close f
  where
    loop = readChan chan >>= handlePw

    handlePw :: FicsMessage -> IO ()
    handlePw Password = get (password wxInputs) text >>= hPutStrLn h
    handlePw (GuestLogin _) = hPutStrLn h ""
    handlePw Login = return ()
    handlePw _ = loop


loginInputs :: Panel () -> IO WxLogin
loginInputs p = WxLogin
  <$> textEntry p [alignment := AlignRight]
  <*> textCtrlEx p wxTE_PASSWORD [alignment := AlignRight]
  <*> checkBox p []

toggleLoginFields :: WxLogin -> IO ()
toggleLoginFields wxLogin = do
  isChecked <- get (guestLogin wxLogin) checked
  set (name wxLogin) [enabled := not isChecked]
  set (password wxLogin) [enabled := not isChecked]
