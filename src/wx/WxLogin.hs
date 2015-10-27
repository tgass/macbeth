module WxLogin (
  wxLogin
) where

import System.IO (Handle, hPutStrLn)
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Graphics.UI.WX
import Graphics.UI.WXCore

import CommandMsg
import WxToolBox


wxLogin :: Handle -> Chan CommandMsg -> IO ()
wxLogin h chan = do
  chan' <- dupChan chan

  f <- frameFixed [ text := "Login to freechess.org" ]
  p <- panel f []
  e_name <- textEntry p [ text := "guest", alignment := AlignRight]
  e_pw   <- textCtrlEx p wxTE_PASSWORD [ alignment := AlignRight]

  status <- statusField []

  b_ok  <- button p [text := "Login", on command := okBtnHandler e_name e_pw status f h chan chan']
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


okBtnHandler :: TextCtrl() -> TextCtrl() -> StatusField -> Frame() -> Handle -> Chan CommandMsg -> Chan CommandMsg -> IO ()
okBtnHandler e_name e_pw status f h chan chan' = do
  name <- get e_name text
  pw <- get e_pw text
  loop name pw False
  where
    loop name pw isGuest = do
       cmd <- readChan chan
       case cmd of
         Login -> hPutStrLn h name >> loop name pw isGuest
         Password -> hPutStrLn h pw >> loop name pw isGuest
         LoggedIn name' -> hPutStrLn h `mapM_` ["set seek 0", "set style 12", "iset nowrap 1", "iset block 1"] >>
                           close f >>
                           createToolBox h name' isGuest chan'
         InvalidPassword  -> void $ set status [text := "Invalid password"]
         UnkownUsername _ -> hPutStrLn h name >> loop name pw isGuest
         GuestLogin _     -> hPutStrLn h "" >> loop name pw True
         _                -> loop name pw isGuest

