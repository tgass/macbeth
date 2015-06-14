module WxLogin (
  wxLogin
) where

import System.IO (Handle, hPutStrLn)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Graphics.UI.WX
import Graphics.UI.WXCore

import CommandMsg
import WxBackground
import WxToolBox


wxLogin :: Handle -> Chan CommandMsg -> IO ()
wxLogin h chan = do
  f <- frameFixed [ text := "Login to freechess.org" ]
  p <- panel f []

  e_name <- textEntry p [ text := "guest", alignment := AlignRight]
  e_pw   <- textCtrlEx p (wxTE_PASSWORD) [ alignment := AlignRight]

  status <- statusField []

  b_ok  <- button p [text := "Login", on command := okBtnHandler e_name e_pw status f h chan ]
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


--TODO: handling von guest logins / unknown usernames verbessern
okBtnHandler :: TextCtrl() -> TextCtrl() -> StatusField -> Frame() -> Handle -> Chan CommandMsg -> IO ()
okBtnHandler e_name e_pw status f h chan = do
  name <- get e_name text
  pw <- get e_pw text
  loop name pw
  where
    loop name pw = do
       cmd <- readChan chan
       case cmd of
         Login -> hPutStrLn h name >> loop name pw
         Password -> hPutStrLn h pw >> loop name pw
         LoggedIn name' -> hPutStrLn h `mapM_` ["set seek 0", "set style 12", "iset nowrap 1", "iset block 1"] >>
                           close f >>
                           dupChan chan >>= createToolBox h name' (name == "guest") >>
                           dupChan chan >>= wxBackground h name'
         InvalidPassword  -> set status [text := "Invalid password"] >> return ()
         UnkownUsername _ -> hPutStrLn h name >> loop name pw
         GuestLogin _     -> hPutStrLn h "" >> loop name pw
         _                -> loop name pw

