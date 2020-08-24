{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Macbeth.Wx.Seek (
    seekFrame
) where


import           Control.Lens hiding (Choice, set)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Maybe
import qualified Data.Map.Strict as Map
import           Graphics.UI.WX hiding (when)
import           Graphics.UI.WXCore hiding (when, Timer, black, white, point)
import qualified Macbeth.Fics.Commands as Cmds
import           Macbeth.Fics.FicsMessage hiding (gameId)
import           Macbeth.Wx.Config.SeekConfig
import qualified Macbeth.Wx.Config.UserConfig as UserConfig
import           Macbeth.Wx.GameType
import qualified Macbeth.Wx.RuntimeEnv as E
import qualified Macbeth.Wx.Utils as Utl
import           System.IO


-- seek [scTime scInc] [scRated|unscRated] [white|black] [crazyhouse] [suicide] [wild #] [auto|scManual] [formula] [rating-range]
type WxSeekConfig = SeekConfig' (Choice ()) (Choice ()) (Choice ()) (TextCtrl ()) (CheckBox ())


seekFrame :: E.RuntimeEnv -> Bool -> Chan FicsMessage -> IO ()
seekFrame env isGuest chan = do
  let h = E.handle env

  f <- frameFixed [ text := "Seek a match" ]
  p <- panel f []

  seekConfig <- readTVarIO $ E.rtSeekConfig env 
  wxSeek <- mkWxSeek seekConfig p isGuest

  set (wxSeek ^. scCategory) [ on select ::= onSelectGameTypeCategory wxSeek ]
  when (isJust $ seekConfig ^. scBoard) $
    set (wxSeek ^. scBoard) [ items := fmap displayBoard $ gameTypes Map.! (seekConfig ^. scCategory)
                            , selection := fromEnum $ fromJust $ seekConfig ^. scBoard ]

  b_ok  <- button p [text := "Create", on command := readSeek wxSeek >>= runSeek h >> saveSeekConfig env wxSeek >> close f ]
  b_can <- button p [text := "Cancel", on command := close f ]

  set f [ defaultButton := b_ok, layout := fill $ container p $ margin 10 $ column 15 [
            boxed "Game Type" (grid 15 15 [
              [label "Category: ", hfill $ widget $ wxSeek ^. scCategory, label "Board:", hfill $ widget $ wxSeek ^. scBoard ]
            ]),
            boxed "" (grid 15 15 [
                  [ label "Time [min.]:", hfill $ widget $ wxSeek ^. scTime, label "Inc [sec.]:", hfill $ widget $ wxSeek ^. scInc ]
                , [ label "Rated:", hfill $ widget $ wxSeek ^. scRated, label "Color:", hfill $ widget $ wxSeek ^. scColor ]
                , [ label "Manual accept:", hfill $ widget $ wxSeek ^. scManual, empty, empty]
                , [ label "Rating from", hfill $ widget $ wxSeek ^. scRatingFrom, label "to", hfill $ widget $ wxSeek ^. scRatingTo ]
              ])
            , floatBottomRight $ row 5 [widget b_can, widget b_ok]]
        ]

  windowOnKeyDown p (\evt -> if
    | Utl.keyWithMod evt 'W' justControl -> close f
    | otherwise -> return ())

  Utl.registerWxCloseEventListener f chan 

saveSeekConfig :: E.RuntimeEnv -> WxSeekConfig -> IO ()
saveSeekConfig env wxSeek = do
  config <- UserConfig.loadConfig
  currentSeekConfig <- readSeek wxSeek
  let updated = config { UserConfig.seekConfig = Just $ convertToFormat currentSeekConfig }
  UserConfig.saveConfig updated
  E.setSeekConfig env currentSeekConfig


runSeek :: Handle -> SeekConfig -> IO ()
runSeek h s = Cmds.seek h (s ^. scTime) (s ^. scInc) (s ^. scRated) (s ^. scColor) (s ^. scCategory) (s ^. scBoard) (s ^. scManual) (s ^. scRatingFrom) (s ^. scRatingTo)

readSeek :: WxSeekConfig -> IO SeekConfig
readSeek s = SeekConfig
  <$> toEnum `fmap` get (s ^. scCategory) selection
  <*> (\idx -> if idx == -1 then Nothing else Just $ toEnum idx) `fmap` get (s ^. scBoard) selection
  <*> toEnum `fmap` get (s ^. scColor) selection
  <*> read `fmap` get (s ^. scTime) text
  <*> read `fmap` get (s ^. scInc) text
  <*> get (s ^. scRated) checked
  <*> get (s ^. scManual) checked
  <*> read `fmap` get (s ^. scRatingFrom) text
  <*> read `fmap` get (s ^. scRatingTo) text


mkWxSeek :: SeekConfig -> Panel () -> Bool -> IO WxSeekConfig
mkWxSeek seekConfig p isGuest = SeekConfig
  <$> choice p [ items := fmap show $ filter (/= Bughouse) $ enumFrom (minBound :: Category) 
               , selection := fromEnum $ seekConfig ^. scCategory ]
  <*> choice p []
  <*> choice p [ tooltip := "scColor"
               , sorted := False
               , items := fmap show $ enumFrom (minBound :: SeekColor)
               , selection := fromEnum $ seekConfig ^. scColor ]
  <*> textEntry p [ text := show $ seekConfig ^. scTime]
  <*> textEntry p [ text := show $ seekConfig ^. scInc]
  <*> checkBox p [ enabled := not isGuest
                 , checked := (not isGuest) && seekConfig ^. scRated ]
  <*> checkBox p [ checked := seekConfig ^. scManual ]
  <*> textEntry p [ text := show $ seekConfig ^. scRatingFrom ]
  <*> textEntry p [ text := show $ seekConfig ^. scRatingTo ]

onSelectGameTypeCategory :: WxSeekConfig -> Choice () -> IO ()
onSelectGameTypeCategory wxSeek scCategoryChoice= do
  category <- toEnum `fmap` get scCategoryChoice selection
  set (wxSeek ^. scBoard)  [ items := fmap displayBoard $ gameTypes Map.! category]

