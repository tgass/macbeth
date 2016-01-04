module Macbeth.Wx.Seek (
  wxSeek
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Wx.GameType
import Macbeth.Wx.Utils

import Control.Applicative hiding (empty)
import Control.Concurrent.Chan
import Data.Char (toLower)
import Graphics.UI.WX hiding (color)
import Graphics.UI.WXCore
import System.IO

eventId = wxID_HIGHEST + 57

-- seek [time inc] [rated|unrated] [white|black] [crazyhouse] [suicide] [wild #] [auto|manual] [formula] [rating-range]
data WxSeek = WxSeek {
    category :: Choice ()
  , board :: Choice ()
  , time :: TextCtrl ()
  , inc :: TextCtrl ()
  , rated :: CheckBox ()
  , color :: Choice ()
  , manual :: CheckBox ()
--, formula :: CheckBox ()
  , ratingFrom :: TextCtrl ()
  , ratingTo :: TextCtrl ()
}


wxSeek :: Handle -> Bool -> Chan FicsMessage -> IO ()
wxSeek h isGuest chan = do
  f <- frameFixed [ text := "Seek a match" ]
  p <- panel f []
  match <- matchInputs p isGuest
  set (category match) [on select ::= onSelectGameTypeCategory (board match)]

  b_ok  <- button p [text := "Create", on command := toString match >>= hPutStrLn h >> close f ]
  b_can <- button p [text := "Cancel", on command := close f]

  set f [ defaultButton := b_ok
        , layout := container p $ margin 10 $ column 15 [
            boxed "Game Type" (grid 15 15 [
              [label "Category: ", hfill $ widget $ category match, label "Board:", hfill $ widget $ board match ]
            ]),
            boxed "" (grid 15 15 [
                  [ label "Time [min.]:", hfill $ widget $ time match, label "Inc [sec.]:", hfill $ widget $ inc match]
                , [ label "Rated:", hfill $ widget $ rated match, label "Color:", hfill $ widget $ color match]
                , [ label "Manual accept:", hfill $ widget $ manual match, empty, empty]
                , [ label "Rating from", hfill $ widget $ ratingFrom match, label "to", hfill $ widget $ ratingTo match]
              ])
            , floatBottomRight $ row 5 [widget b_can, widget b_ok]]
        ]
  registerWxCloseEventListener chan eventId f


-- seek [time inc] [rated|unrated] [white|black] [crazyhouse] [suicide]
--      [wild #] [auto|manual] [formula] [rating-range]
toString:: WxSeek -> IO String
toString m = (("4 seek " ++) . unwords) `fmap` sequence [
    get (time m) text
  , get (inc m) text
  , convertIsRated <$> get (rated m) enabled
  , get (color m) selection >>= fmap convertColor . get (color m) . item
  , gameTypeIdxToString <$> get (category m) selection <*> get (board m) selection
  , convertAutoManual <$> get (manual m) checked
  , convertRatingRange <$> get (ratingFrom m) text <*> get (ratingTo m) text]
    where
      convertIsRated r = if r then "rated" else "unrated"
      convertColor x = if x == "Automatic" then "" else fmap toLower x
      convertRatingRange from to = from ++ "-" ++ to
      convertAutoManual isManual = if isManual then "m" else "a"


matchInputs :: Panel () -> Bool -> IO WxSeek
matchInputs p isGuest = WxSeek
  <$> choice p [items := fmap (show . fst) gameTypes]
  <*> choice p []
  <*> textEntry p [ text := "5"]
  <*> textEntry p [ text := "0"]
  <*> checkBox p [ enabled := not isGuest ]
  <*> choice p [tooltip := "color", sorted := False, items := ["Automatic", "White", "Black"]]
  <*> checkBox p []
  <*> textEntry p [ text := "0"]
  <*> textEntry p [ text := "9999"]


