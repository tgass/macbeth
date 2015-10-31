module Lentils.Wx.Seek (
  wxSeek,
  main
) where

import Control.Applicative
import Control.Monad (void)
import Data.Char (toLower)
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO (Handle, hPutStrLn, stdout)

main = start $ wxSeek stdout True


wxSeek :: Handle -> Bool -> IO ()
wxSeek h isGuest = do
  f <- frameFixed [ text := "Seek a match" ]
  p <- panel f []
  match <- matchInputs p isGuest

  b_ok  <- button p [text := "Create", on command := toString match >>= hPutStrLn h >> close f ]
  b_can <- button p [text := "Cancel", on command := close f]

  set f [ defaultButton := b_ok
        , layout := container p $ margin 10 $
            column 10 [boxed "" (
              grid 15 15 [
                [ label "Time [min.]:", hfill $ widget $ time match, label "Inc [sec.]:", hfill $ widget $ inc match]
              , [ label "Rated:", hfill $ widget $ rated match, label "Color:", hfill $ widget $ Lentils.Wx.Seek.color match]
              , [ label "Rating from", hfill $ widget $ ratingFrom match, label "to", hfill $ widget $ ratingTo match]
              ])
            , floatBottomRight $ row 5 [widget b_can, widget b_ok]]
        ]
  void $ windowShow f

-- 4 seek time inc rated color from-to
toString:: WxSeek -> IO String
toString m = (("4 seek " ++) . unwords) `fmap` sequence [
    get (time m) text
  , get (inc m) text
  , convertIsRated `fmap` get (rated m) enabled
  , get (Lentils.Wx.Seek.color m) selection >>= fmap convertColor . get (Lentils.Wx.Seek.color m) . item
  , convertRatingRange <$> get (ratingFrom m) text <*> get (ratingTo m) text]
    where
      convertIsRated True = "rated"
      convertIsRated False = "unrated"
      convertColor "Automatic" = ""
      convertColor x = fmap toLower x
      convertRatingRange from to
             | from == "" && to == "" = ""
             | otherwise = from ++ "-" ++ to


matchInputs :: Panel () -> Bool -> IO WxSeek
matchInputs p isGuest = WxSeek
  <$> textEntry p [ text := "5"]
  <*> textEntry p [ text := "0"]
  <*> checkBox p [ enabled := not isGuest ]
  <*> choice p [tooltip := "color", sorted := False, items := ["Automatic", "White", "Black"]]
  <*> textEntry p [ text := "0"]
  <*> textEntry p [ text := "9999"]


-- seek [time inc] [rated|unrated] [white|black] [crazyhouse] [suicide]
--                 [wild #] [auto|manual] [formula] [rating-range]
data WxSeek = WxSeek {
  time :: TextCtrl (),
  inc :: TextCtrl (),
  rated :: CheckBox (),
  color :: Choice (),
  --manual :: CheckBox ()
--  formula :: CheckBox (),
  ratingFrom :: TextCtrl (),
  ratingTo :: TextCtrl ()
}
