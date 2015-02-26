module WxLogin (
  login
) where

import System.IO (Handle, hPutStrLn)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Graphics.UI.WX

import WxToolBox
import CommandMsg


login :: Handle -> Chan CommandMsg -> IO ()
login h chan = do
  f <- frame []
  p <- panel f []

  e_name <- textEntry p [text := "100", alignment := AlignRight]
  e_pw <- textEntry p [text := "100", alignment := AlignRight]

  b_ok <- button p [text := "Ok", on command := loginLoop f h chan ]

  b_can <- button p [text := "Cancel", on command := infoDialog f "Info" "Pressed 'Cancel'"]

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



loginLoop :: Frame () -> Handle -> Chan CommandMsg -> IO ()
loginLoop f h chan = do
    cmd <- readChan chan
    case cmd of
      LoginMessage     -> hPutStrLn h "guest" >>
                          loginLoop f h chan

      PasswordMessage  -> hPutStrLn h "efgeon" >>
                          loginLoop f h chan

      LoggedInMessage  -> hPutStrLn h "set seek 0" >>
                          hPutStrLn h "set style 12" >>
                          hPutStrLn h "iset nowrap 1" >>
                          hPutStrLn h "iset block 1" >>
                          close f >>
                          dupChan chan >>= createToolBox h

      GuestLoginMsg _  -> hPutStrLn h "" >>
                          loginLoop f h chan

      _ -> loginLoop f h chan
