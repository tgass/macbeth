module Macbeth.Wx.Seek (
    wxSeek
  , toString
  , SeekInfo (..)
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.FicsMessage
import Macbeth.Wx.GameType
import Macbeth.Wx.Utils

import Control.Concurrent.Chan
import Control.Monad.Cont
import Data.Map (keys)
import Graphics.UI.WX hiding (color)
import Safe
import System.IO

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

data SeekInfo = SeekInfo {
    _category :: Category
  , _board :: Maybe Board
  , _time :: Int
  , _inc :: Int
  , _rated :: Bool
  , _color :: Maybe PColor
  , _manual :: Bool
--  , _formula :: Bool
  , _ratingFrom :: Int
  , _ratingTo :: Int
}

wxSeek :: Handle -> Bool -> Chan FicsMessage -> IO ()
wxSeek h isGuest chan = runCont (basicFrame frameConfig chan) $ setupFrame h isGuest


frameConfig :: FrameConfig
frameConfig = FrameConfig {
  fCreate = frameFixed,
  fTitle = "Seek a match",
  hasStatusField = False
}


setupFrame :: Handle -> Bool -> (Panel (), StatusField, FrameActions) -> IO ()
setupFrame h isGuest (p, _, frame') = do
  match <- matchInputs p isGuest
  set (category match) [on select ::= onSelectGameTypeCategory (board match)]

  b_ok  <- button p [text := "Create", on command := readSeek match >>= hPutStrLn h . ("4 seek " ++) . toString >> closeFrame frame' ]
  b_can <- button p [text := "Cancel", on command := closeFrame frame']

  frame' `setDefaultButton` b_ok
  set p [ layout := container p $ margin 10 $ column 15 [
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


-- seek [time inc] [rated|unrated] [white|black] [crazyhouse] [suicide]
--      [wild #] [auto|manual] [formula] [rating-range]
toString :: SeekInfo -> String
toString m = unwords $ filter (/= "") [
    show $ _time m
  , show $ _inc m
  , convertIsRated $ _rated m
  , maybe "" convertColor (_color m)
  , gameTypeSelectionToString (_category m) (_board m)
  , convertAutoManual (_manual m)
  , convertRatingRange (_ratingFrom m) (_ratingTo m)]
    where
      convertIsRated r = if r then "r" else "u"
      convertColor White = "w"
      convertColor Black = "b"
      convertRatingRange from to = show from ++ "-" ++ show to
      convertAutoManual isManual = if isManual then "m" else "a"


readSeek :: WxSeek -> IO SeekInfo
readSeek m = SeekInfo
  <$> read <$> getDisplaySelection (category m)
  <*> readMay `fmap` getDisplaySelection (board m)
  <*> read `fmap` get (time m) text
  <*> read `fmap` get (inc m) text
  <*> get (rated m) checked
  <*> convertColor `fmap` getDisplaySelection (color m)
  <*> get (manual m) checked
  <*> read `fmap` get (ratingFrom m) text
  <*> read `fmap` get (ratingTo m) text
  where
    convertColor "Automatic" = Nothing
    convertColor c = read c


matchInputs :: Panel () -> Bool -> IO WxSeek
matchInputs p isGuest = WxSeek
  <$> choice p [items := fmap show (filter (/= Bughouse) $ keys gameTypes)]
  <*> choice p []
  <*> textEntry p [ text := "5"]
  <*> textEntry p [ text := "0"]
  <*> checkBox p [ enabled := not isGuest ]
  <*> choice p [tooltip := "color", sorted := False, items := ["Automatic", "White", "Black"]]
  <*> checkBox p []
  <*> textEntry p [ text := "0"]
  <*> textEntry p [ text := "9999"]

