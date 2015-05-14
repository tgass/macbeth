module WxSeek (
  wxSeek,
  main
) where

import Control.Monad (liftM)
import Graphics.UI.WX
import Graphics.UI.WXCore (windowShow)
import System.IO (Handle, hPutStrLn, stdout)

main = start $ wxSeek stdout

wxSeek :: Handle -> IO ()
wxSeek h = do
  f <- frame []
  p <- panel f []
  match <- matchInputs p

  b_ok  <- button p [text := "Challenge", on command := toString match >>= hPutStrLn h >> close f ]
  b_can <- button p [text := "Cancel", on command := close f]

  set f [ defaultButton := b_ok
        , layout := container p $ margin 10 $
            column 5 [
              grid 5 5 [
                [ label "Rated:", hfill $ widget $ rated match]
               ,[ label "Time:", hfill $ widget $ time match]
               ,[ label "Inc:", hfill $ widget $ inc match]
              ]
            , floatBottomRight $ row 5 [widget b_can, widget b_ok]]
        ]
  windowShow f
  return ()

toString:: WxSeek -> IO String
toString m = do
  time <- get (time m) text
  inc <- get (inc m) text
  rated <- convertIsRated `liftM` get (rated m) enabled
  return $ "4 seek " ++ " " ++ time ++ " " ++ inc ++ " " ++ rated
    where
      convertIsRated True = "rated"
      convertIsRated False = "unrated"

matchInputs :: Panel () -> IO WxSeek
matchInputs p = do
  time <- textEntry p [ text := "5"]
  inc <- textEntry p [ text := "0"]
  rated <- checkBox p []
  color <- choice p [tooltip := "color", sorted := False, items := ["Automatic", "White", "Black"]]
  return $ WxSeek time inc rated color

-- seek [time inc] [rated|unrated] [white|black] [crazyhouse] [suicide]
--                 [wild #] [auto|manual] [formula] [rating-range]
data WxSeek = WxSeek {
  time :: TextCtrl (),
  inc :: TextCtrl (),
  rated :: CheckBox (),
  color :: Choice ()
--  auto :: CheckBox (),
--  formula :: CheckBox (),
--  ratingFrom :: TextCtrl (),
--  ratingTo :: TextCtrl ()
}
