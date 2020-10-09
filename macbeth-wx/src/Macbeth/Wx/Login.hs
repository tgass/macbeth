module Macbeth.Wx.Login (
  wxLogin
) where

import           Control.Concurrent.Chan
import           Control.Monad
import           Control.Monad.Reader
import           Graphics.UI.WX hiding (when)
import           Graphics.UI.WXCore hiding (when)
import           Macbeth.Fics.Message
import qualified Macbeth.Fics.Commands as Cmds
import qualified Macbeth.Wx.Config.UserConfig as Config
import           Macbeth.Wx.Utils
import           Macbeth.Wx.RuntimeEnv hiding (username)

data Login' a b = Login {
    username :: a
  , password :: a
  , guestLogin :: b
  , saveCredentials :: b
}

type WxLogin = Login' (TextCtrl ()) (CheckBox ())
type LoginData = Login' String Bool


wxLogin :: RuntimeEnv -> Chan Message -> IO ()
wxLogin env chan = do
  f <- frameFixed [ text := "Macbeth" ]
  p <- panel f []
  wxInputs <- loginInputs p
  set (guestLogin wxInputs) [on command := toggleLoginFields wxInputs]

  b_ok  <- button p [text := "Login", on command := okBtnHandler wxInputs f env chan]
  b_can <- button p [text := "Quit", on command := close f]

  set f [ defaultButton := b_ok
        , layout := container p $ margin 10 $ column 25 [
              boxed "Login @ freechess.org" (grid 15 15 [
                 [ label "Username:", hfill $ widget $ username wxInputs]
               , [ label "Password:", hfill $ widget $ password wxInputs]
               , [ label "Login as Guest:", hfill $ widget $ guestLogin wxInputs]
               , [ label "Auto-Login:", hfill $ widget $ saveCredentials wxInputs]])
            , floatBottomRight $ row 5 [widget b_ok, widget b_can]]
        ]
  dupChan chan >>= registerWxCloseEventListener f


okBtnHandler :: WxLogin -> Frame () -> RuntimeEnv -> Chan Message -> IO ()
okBtnHandler wxInputs f env chan = do
  loginData <- extract wxInputs
  flip runReaderT loginData $ do
    putUsername 
    putPassword 
    lift $ close f
  where
    putUsername :: ReaderT LoginData IO ()
    putUsername = asks usernameOrGuest >>= liftIO . Cmds.message env

    -- TODO: do this with `fix`
    putPassword :: ReaderT LoginData IO ()
    putPassword = lift (readChan chan) >>= \case
      AbusiveBehavior -> return ()
      Password -> asks password >>= lift . Cmds.message env >> putPassword
      GuestLogin {} -> lift (Cmds.message env "") >> putPassword
      LoginPrompt -> return () -- close this frame, new one is opened in Toolbox
      LoggedIn {} -> ask >>= \login -> lift $
        when (saveCredentials login) $ Config.saveCredentials (usernameOrGuest login) (password login)
      _ -> putPassword


usernameOrGuest :: LoginData -> String
usernameOrGuest login 
  | guestLogin login = "guest"
  | otherwise = username login


extract :: WxLogin -> IO LoginData
extract w = Login
  <$> get (username w) text
  <*> get (password w) text
  <*> get (guestLogin w) checked
  <*> get (saveCredentials w) checked


loginInputs :: Panel () -> IO WxLogin
loginInputs p = Login
  <$> textEntry p [alignment := AlignRight]
  <*> textCtrlEx p wxTE_PASSWORD [alignment := AlignRight]
  <*> checkBox p []
  <*> checkBox p []


toggleLoginFields :: WxLogin -> IO ()
toggleLoginFields w = do
  isChecked <- get (guestLogin w) checked
  set (username w) [enabled := not isChecked]
  set (password w) [enabled := not isChecked]

