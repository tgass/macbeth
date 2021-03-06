module Macbeth.Wx.Match (
  wxMatch
) where

import           Control.Concurrent.Chan
import           Control.Monad.Cont
import           Data.Map
import           Graphics.UI.WX hiding (color)
import           Macbeth.Fics.Api.Seek (SeekColor)
import           Macbeth.Fics.Api.GameType
import qualified Macbeth.Fics.Commands as Cmds
import           Macbeth.Fics.Message
import           Macbeth.Wx.Utils
import           Macbeth.Wx.RuntimeEnv

data WxMatch = WxMatch {
  category :: Choice (),
  board :: Choice (),
  name :: TextCtrl (),
  time :: TextCtrl (),
  inc :: TextCtrl (),
  rated :: CheckBox (),
  color :: Choice ()
}

wxMatch :: RuntimeEnv -> Bool -> Chan Message -> IO ()
wxMatch env isGuest chan = runCont (basicFrame frameConfig chan) $ setupFrame env isGuest


frameConfig :: FrameConfig
frameConfig = FrameConfig {
  fCreate = frameFixed,
  fTitle = "Create a match",
  hasStatusField = False
}


setupFrame :: RuntimeEnv -> Bool -> (Panel (), StatusField, FrameActions) -> IO ()
setupFrame env isGuest (p, _, f) = do
  match <- matchInputs p isGuest
  set (category match) [on select ::= onSelectGameTypeCategory (board match)]

  b_ok  <- button p [text := "Match", on command := startMatch env match >> closeFrame f ]
  b_can <- button p [text := "Cancel", on command := closeFrame f]

  setDefaultButton f b_ok
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
  <*> choice p [ tooltip := "color", sorted := False
               , items := fmap show $ enumFrom (minBound :: SeekColor)]


startMatch :: RuntimeEnv -> WxMatch -> IO ()
startMatch  env m = join $ Cmds.match2 env
     <$> get (name m) text
     <*> get (rated m) checked
     <*> read `fmap` get (time m) text
     <*> read `fmap` get (inc m) text
     <*> read `fmap` getDisplaySelection (color m)
     <*> read `fmap` getDisplaySelection (category m)
     <*> fmap (fmap read) (getMayDisplaySelection (board m))

onSelectGameTypeCategory :: Choice () -> Choice () -> IO ()
onSelectGameTypeCategory c_boards c_category = do
  category' <- fmap read (getDisplaySelection c_category)
  set c_boards [ items := fmap displayBoard $ gameTypes ! category']

