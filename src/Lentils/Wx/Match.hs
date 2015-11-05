module Lentils.Wx.Match (
  main,
  wxMatch
) where

import Control.Applicative
import Data.Char (toLower)
import Graphics.UI.WX
import System.IO (Handle, hPutStrLn, stdout)

data WxMatch = WxMatch {
  name :: TextCtrl (),
  time :: TextCtrl (),
  inc :: TextCtrl (),
  rated :: CheckBox (),
  color :: Choice ()
}

main = start $ wxMatch stdout False

wxMatch :: Handle -> Bool -> IO ()
wxMatch h isGuest = do
  f <- frameFixed [ text := "Create a match" ]
  p <- panel f []
  match <- matchInputs p isGuest

  b_ok  <- button p [text := "Match", on command := toString match >>= hPutStrLn h >> close f ]
  b_can <- button p [text := "Cancel", on command := close f]

  set f [ defaultButton := b_ok
        , layout := container p $ margin 10 $
            column 10 [ boxed "" (
              grid 15 15 [
                [ label "Player name:", hfill $ widget $ name match]
               ,[ label "Rated:", hfill $ widget $ rated match]
               ,[ label "Time:", hfill $ widget $ time match]
               ,[ label "Inc:", hfill $ widget $ inc match]
               ,[ label "Color:", hfill $ widget $ Lentils.Wx.Match.color match]
              ])
            , floatBottomRight $ row 5 [widget b_can, widget b_ok]]
        ]


matchInputs :: Panel () -> Bool -> IO WxMatch
matchInputs p isGuest = WxMatch
  <$> textEntry p [ ]
  <*> textEntry p [ text := "5" ]
  <*> textEntry p [ text := "0" ]
  <*> checkBox p [ enabled := not isGuest ]
  <*> choice p [tooltip := "color", sorted := False, items := ["Automatic", "White", "Black"]]


-- 4 match GuestXYZZ rated 5 0 white
toString:: WxMatch -> IO String
toString m = (("4 match " ++) . unwords) `fmap` sequence [
       get (name m) text
     , convertIsRated `fmap` get (rated m) enabled
     , get (time m) text
     , get (inc m) text
     , get (Lentils.Wx.Match.color m) selection >>= fmap convertColor . get (Lentils.Wx.Match.color m) . item]
    where
      convertIsRated True = "rated"
      convertIsRated False = "unrated"
      convertColor "Automatic" = ""
      convertColor x = fmap toLower x
