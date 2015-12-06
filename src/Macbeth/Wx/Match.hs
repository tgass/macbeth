module Macbeth.Wx.Match (
  main,
  wxMatch
) where

import Macbeth.Api.CommandMsg
import Macbeth.Wx.GameType
import Macbeth.Wx.Utils

import Control.Applicative
import Control.Concurrent.Chan
import Data.Char
import Graphics.UI.WX hiding (color)
import Graphics.UI.WXCore
import System.IO

data WxMatch = WxMatch {
  category :: Choice (),
  board :: Choice (),
  name :: TextCtrl (),
  time :: TextCtrl (),
  inc :: TextCtrl (),
  rated :: CheckBox (),
  color :: Choice ()
}

eventId = wxID_HIGHEST + 56

main = start $ wxMatch stdout False undefined

wxMatch :: Handle -> Bool -> Chan CommandMsg -> IO ()
wxMatch h isGuest chan = do
  f <- frameFixed [ text := "Create a match" ]
  p <- panel f []
  match <- matchInputs p isGuest
  set (category match) [on select ::= onSelectGameTypeCategory (board match)]

  b_ok  <- button p [text := "Match", on command := toString match >>= hPutStrLn h >> close f ]
  b_can <- button p [text := "Cancel", on command := close f]

  set f [ defaultButton := b_ok
        , layout := container p $ margin 10 $ column 10 [
               boxed "Game Type" (grid 15 15 [
                   [label "Category: ", hfill $ widget $ category match]
                 , [label "Board:" , hfill $ widget $ board match ]])
             , boxed "" (grid 15 15 [
                [ label "Player name:", hfill $ widget $ name match]
               ,[ label "Rated:", hfill $ widget $ rated match]
               ,[ label "Time:", hfill $ widget $ time match]
               ,[ label "Inc:", hfill $ widget $ inc match]
               ,[ label "Color:", hfill $ widget $ color match]
              ])
            , floatBottomRight $ row 5 [widget b_can, widget b_ok]]
        ]
  registerWxCloseEventListener chan eventId f


matchInputs :: Panel () -> Bool -> IO WxMatch
matchInputs p isGuest = WxMatch
  <$> choice p [items := fmap (show . fst) gameTypes]
  <*> choice p []
  <*> textEntry p [ ]
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
     , get (color m) selection >>= fmap convertColor . get (color m) . item
     , gameTypeIdxToString <$> get (category m) selection <*> get (board m) selection]
    where
      convertIsRated r = if r then "rated" else "unrated"
      convertColor x = if x == "Automatic" then "" else fmap toLower x
