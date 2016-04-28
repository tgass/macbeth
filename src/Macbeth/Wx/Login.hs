{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.Login (
  wxLogin
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Wx.Utils
import qualified Macbeth.Wx.UserConfig as Config

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Reader
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import System.IO

data WxLogin = WxLogin {
    username :: TextCtrl ()
  , password :: TextCtrl ()
  , guestLogin :: CheckBox ()
  , saveCredentials :: CheckBox ()
}

data LoginData = LoginData {
    username' :: String
  , password' :: String
  , guestLogin' :: Bool
  , saveCredentials' :: Bool
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
                [ label "Username:", hfill $ widget $ username wxInputs]
               ,[ label "Password:", hfill $ widget $ password wxInputs]
               ,[ label "Login as Guest:", hfill $ widget $ guestLogin wxInputs]
               ,[ label "Auto-Login:", hfill $ widget $ saveCredentials wxInputs]])
            , floatBottomRight $ row 5 [widget b_ok, widget b_can]]
        ]
  dupChan chan >>= registerWxCloseEventListener f


okBtnHandler :: WxLogin -> Frame () -> Handle -> Chan FicsMessage -> IO ()
okBtnHandler wxInputs f h chan = do
  loginData' <- loginData wxInputs
  runReaderT (putUsername >> putPassword) loginData' >> close f
  where
    putUsername = usernameOrGuest <$> ask >>= lift . hPutStrLn h

    putPassword = lift (readChan chan) >>= \case
      Password -> password' <$> ask >>= lift . hPutStrLn h >> putPassword
      GuestLogin {} -> liftIO (hPutStrLn h "") >> putPassword
      Login -> return () -- close this frame, new one is opened in Toolbox
      LoggedIn {} -> ask >>= \login -> liftIO $
        when (saveCredentials' login) $ Config.saveCredentials (usernameOrGuest login) (password' login)
      _ -> putPassword


usernameOrGuest :: LoginData -> String
usernameOrGuest login = if guestLogin' login then "guest" else username' login


loginData :: WxLogin -> IO LoginData
loginData wxlogin = LoginData
  <$> get (username wxlogin) text
  <*> get (password wxlogin) text
  <*> get (guestLogin wxlogin) checked
  <*> get (saveCredentials wxlogin) checked


loginInputs :: Panel () -> IO WxLogin
loginInputs p = WxLogin
  <$> textEntry p [alignment := AlignRight]
  <*> textCtrlEx p wxTE_PASSWORD [alignment := AlignRight]
  <*> checkBox p []
  <*> checkBox p []


toggleLoginFields :: WxLogin -> IO ()
toggleLoginFields wxLogin = do
  isChecked <- get (guestLogin wxLogin) checked
  set (username wxLogin) [enabled := not isChecked]
  set (password wxLogin) [enabled := not isChecked]
