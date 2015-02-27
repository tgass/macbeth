module WxLogin (
  login
) where

import System.IO (Handle, hPutStrLn)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Graphics.UI.WX
import Graphics.UI.WXCore

import WxToolBox
import CommandMsg


login :: Handle -> Chan CommandMsg -> IO ()
login h chan = do
  f <- frame []
  p <- panel f []

  e_name <- textEntry p [ text := "guest", alignment := AlignRight]
  e_pw <- textCtrlEx p (wxTE_PASSWORD) [ alignment := AlignRight]


  let foobar = do
                  name <- get e_name text
                  pw <- get e_pw text
                  loginLoop name pw f h chan

  b_ok <- button p [text := "Login", on command := foobar ]
  b_can <- button p [text := "Quit", on command := close f]

  set f [ defaultButton := b_ok
        , layout := container p $ margin 10 $
            column 5 [boxed "Login to freechess.org" (
              grid 5 5 [
                [ label "Username:", hfill $ widget e_name]
               ,[ label "Password:", hfill $ widget e_pw]]
            )
        , floatBottomRight $ row 5 [widget b_ok, widget b_can]]
        ]
  return ()



loginLoop :: String -> String -> Frame () -> Handle -> Chan CommandMsg -> IO ()
loginLoop name pw f h chan = do
    cmd <- readChan chan
    putStrLn $ show cmd
    case cmd of
      LoginMessage     -> hPutStrLn h name >>
                          loginLoop name pw f h chan

      PasswordMessage  -> hPutStrLn h pw >>
                          loginLoop name pw f h chan

      LoggedInMessage name' -> hPutStrLn h "set seek 0" >>
                               hPutStrLn h "set style 12" >>
                               hPutStrLn h "iset nowrap 1" >>
                               hPutStrLn h "iset block 1" >>
                               close f >>
                               dupChan chan >>= createToolBox h name'

      InvalidPasswordMsg  -> return ()

      UnkownUsernameMsg _ -> hPutStrLn h name >>
                             loginLoop name pw f h chan

      GuestLoginMsg _  -> hPutStrLn h "" >>
                          loginLoop name pw f h chan

      _ -> loginLoop name pw f h chan
