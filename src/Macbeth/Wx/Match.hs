module Macbeth.Wx.Match (
  wxMatch
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Wx.GameType
import Macbeth.Wx.Utils

import Control.Concurrent.Chan
import Control.Monad.Cont
import Data.Map
import Data.Char
import Graphics.UI.WX hiding (color)
import Safe
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

wxMatch :: Handle -> Bool -> Chan FicsMessage -> IO ()
wxMatch h isGuest chan = runCont (basicFrame frameConfig chan) $ setupFrame h isGuest


frameConfig :: FrameConfig
frameConfig = FrameConfig {
  fCreate = frameFixed,
  fTitle = "Create a match",
  hasStatusField = False
}


setupFrame :: Handle -> Bool -> (Panel (), StatusField, FrameActions) -> IO ()
setupFrame h isGuest (p, _, frame') = do
  match <- matchInputs p isGuest
  set (category match) [on select ::= onSelectGameTypeCategory (board match)]

  b_ok  <- button p [text := "Match", on command := toString match >>= hPutStrLn h >> closeFrame frame' ]
  b_can <- button p [text := "Cancel", on command := closeFrame frame']

  frame' `setDefaultButton` b_ok
  set p [ layout := margin 10 $ column 10 [
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


matchInputs :: Panel () -> Bool -> IO WxMatch
matchInputs p isGuest = WxMatch
  <$> choice p [ items := fmap show (keys gameTypes)]
  <*> choice p []
  <*> textEntry p [ ]
  <*> textEntry p [ text := "5" ]
  <*> textEntry p [ text := "0" ]
  <*> checkBox p [ enabled := not isGuest ]
  <*> choice p [ tooltip := "color", sorted := False, items := ["Automatic", "White", "Black"]]


-- 4 match GuestXYZZ rated 5 0 white
toString:: WxMatch -> IO String
toString m = (("4 match " ++) . unwords) `fmap` sequence [
       get (name m) text
     , convertIsRated `fmap` get (rated m) enabled
     , get (time m) text
     , get (inc m) text
     , get (color m) selection >>= fmap convertColor . get (color m) . item
     , gameTypeSelectionToString <$> read `fmap` getDisplaySelection (category m)
                                 <*> readMay `fmap` getDisplaySelection (board m)]
    where
      convertIsRated r = if r then "rated" else "unrated"
      convertColor x = if x == "Automatic" then "" else fmap toLower x
