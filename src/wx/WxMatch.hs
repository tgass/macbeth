module WxMatch (
  main,
  wxMatch
) where

import Control.Applicative
import Control.Monad (sequence)
import Data.Char (toLower)
import Data.List (intersperse)
import Graphics.UI.WX
import Graphics.UI.WXCore (windowShow)
import System.IO (Handle, hPutStrLn, stdout)

main = start $ wxMatch stdout

wxMatch :: Handle -> IO ()
wxMatch h = do
  f <- frame []
  p <- panel f []
  match <- matchInputs p

  b_ok  <- button p [text := "Match", on command := toString match >>= hPutStrLn h >> close f ]
  b_can <- button p [text := "Cancel", on command := close f]

  set f [ defaultButton := b_ok
        , layout := container p $ margin 10 $
            column 10 [ boxed "Create new match" (
              grid 15 15 [
                [ label "Player name:", hfill $ widget $ name match]
               ,[ label "Rated:", hfill $ widget $ rated match]
               ,[ label "Time:", hfill $ widget $ time match]
               ,[ label "Inc:", hfill $ widget $ inc match]
               ,[ label "Color:", hfill $ widget $ WxMatch.color match]
              ])
            , floatBottomRight $ row 5 [widget b_can, widget b_ok]]
        ]
  windowShow f
  return ()


matchInputs :: Panel () -> IO WxMatch
matchInputs p = WxMatch
  <$> textEntry p []
  <*> textEntry p [ text := "5"]
  <*> textEntry p [ text := "0"]
  <*> checkBox p []
  <*> choice p [tooltip := "color", sorted := False, items := ["Automatic", "White", "Black"]]


-- 4 match GuestXYZZ rated 5 0 white
toString:: WxMatch -> IO String
toString m = (("4 match " ++) . concat . intersperse " ") `fmap` sequence [
       get (name m) text
     , convertIsRated `fmap` get (rated m) enabled
     , get (time m) text
     , get (inc m) text
     , get (WxMatch.color m) selection >>= fmap convertColor . (get $ WxMatch.color m) . item]
    where
      convertIsRated True = "rated"
      convertIsRated False = "unrated"
      convertColor "Automatic" = ""
      convertColor x = fmap toLower x


data WxMatch = WxMatch {
  name :: TextCtrl (),
  time :: TextCtrl (),
  inc :: TextCtrl (),
  rated :: CheckBox (),
  color :: Choice ()
}
