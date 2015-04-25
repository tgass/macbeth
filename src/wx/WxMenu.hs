module WxMenu (
  wxMenu
) where

import Control.Monad (liftM)
import Graphics.UI.WX
import Graphics.UI.WXCore (windowShow)
import System.IO (Handle, hPutStrLn)



wxMenu :: Handle -> IO (Menu())
wxMenu h = do
  actions <- menuPane [text := "Actions"]
  match <- menuItem actions [text := "Match", on command := wxMatch h]
  return actions


wxMatch :: Handle -> IO ()
wxMatch h = do
  f <- frame []
  p <- panel f []
  match <- matchInputs p

  b_ok  <- button p [text := "Challenge", on command := toString match >>= hPutStrLn h >> close f ]
  b_can <- button p [text := "Cancel", on command := close f]

  set f [ defaultButton := b_ok
        , layout := container p $ margin 10 $
            column 5 [
              grid 5 5 [
                [ label "Player name:", hfill $ widget $ name match]
               ,[ label "Rated:", hfill $ widget $ rated match]
               ,[ label "Time:", hfill $ widget $ time match]
               ,[ label "Inc:", hfill $ widget $ inc match]
              ]
            , floatBottomRight $ row 5 [widget b_can, widget b_ok]]
        ]
  windowShow f
  return ()


matchInputs :: Panel () -> IO Match
matchInputs p = do
  name <- textEntry p []
  time <- textEntry p [ text := "5"]
  inc <- textEntry p [ text := "0"]
  rated <- checkBox p []
  return $ Match name time inc rated


toString:: Match -> IO String
toString m = do
  name <- get (name m) text
  time <- get (time m) text
  inc <- get (inc m) text
  rated <- convertIsRated `liftM` get (rated m) enabled
  return $ "4 match " ++ name ++ " " ++ time ++ " " ++ inc ++ " " ++ rated
    where
      convertIsRated True = "rated"
      convertIsRated False = "unrated"


data Match = Match {
  name :: TextCtrl (),
  time :: TextCtrl (),
  inc :: TextCtrl (),
  rated :: CheckBox ()
}
