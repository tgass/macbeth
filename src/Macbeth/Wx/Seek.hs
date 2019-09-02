{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Macbeth.Wx.Seek (
    seekFrame
) where


import qualified Control.Lens as L
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Maybe
import qualified Data.Map.Strict as Map
import           Graphics.UI.WX hiding (when)
import           Graphics.UI.WXCore hiding (when, Timer, black, white, point)
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

  set (wxSeek L.^. scCategory) [ on select ::= onSelectGameTypeCategory wxSeek ]
  when (isJust $ seekConfig L.^. scBoard) $
    set (wxSeek L.^. scBoard) [ items := fmap displayBoard $ gameTypes Map.! (seekConfig L.^. scCategory)
                            , selection := fromEnum $ fromJust $ seekConfig L.^. scBoard ]

  b_ok  <- button p [text := "Create", on command := readSeek wxSeek >>= hPutStrLn h . ("4 seek " ++) . toString >> saveSeekConfig env wxSeek >> close f ]
  b_can <- button p [text := "Cancel", on command := close f ]

  set f [ defaultButton := b_ok, layout := fill $ container p $ margin 10 $ column 15 [
            boxed "Game Type" (grid 15 15 [
              [label "Category: ", hfill $ widget $ wxSeek L.^. scCategory, label "Board:", hfill $ widget $ wxSeek L.^. scBoard ]
            ]),
            boxed "" (grid 15 15 [
                  [ label "Time [min.]:", hfill $ widget $ wxSeek L.^. scTime, label "Inc [sec.]:", hfill $ widget $ wxSeek L.^. scInc ]
                , [ label "Rated:", hfill $ widget $ wxSeek L.^. scRated, label "Color:", hfill $ widget $ wxSeek L.^. scColor ]
                , [ label "Manual accept:", hfill $ widget $ wxSeek L.^. scManual, empty, empty]
                , [ label "Rating from", hfill $ widget $ wxSeek L.^. scRatingFrom, label "to", hfill $ widget $ wxSeek L.^. scRatingTo ]
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


-- seek [scTime scInc] [scRated|unscRated] [white|black] [crazyhouse] [suicide]
--      [wild #] [auto|scManual] [formula] [rating-range]
toString :: SeekConfig -> String
toString s = unwords $ filter (/= "") [
    show $ s L.^. scTime
  , show $ s L.^. scInc
  , convertIsRated $ s L.^. scRated
  , convertColor $ s L.^. scColor
  , gameTypeSelectionToString (s L.^. scCategory) (s L.^. scBoard)
  , convertIsManual $ s L.^. scManual
  , convertRatingRange (s L.^. scRatingFrom) (s L.^. scRatingTo)]
    where
      convertIsRated True = "r"
      convertIsRated False = "u"
      convertColor White = "w"
      convertColor Black = "b"
      convertColor Automatic = ""
      convertRatingRange from to = show from ++ "-" ++ show to
      convertIsManual True = "m"
      convertIsManual False = "a"


readSeek :: WxSeekConfig -> IO SeekConfig
readSeek s = SeekConfig
  <$> toEnum `fmap` get (s L.^. scCategory) selection
  <*> (\idx -> if idx == -1 then Nothing else Just $ toEnum idx) `fmap` get (s L.^. scBoard) selection
  <*> toEnum `fmap` get (s L.^. scColor) selection
  <*> read `fmap` get (s L.^. scTime) text
  <*> read `fmap` get (s L.^. scInc) text
  <*> get (s L.^. scRated) checked
  <*> get (s L.^. scManual) checked
  <*> read `fmap` get (s L.^. scRatingFrom) text
  <*> read `fmap` get (s L.^. scRatingTo) text


mkWxSeek :: SeekConfig -> Panel () -> Bool -> IO WxSeekConfig
mkWxSeek seekConfig p isGuest = SeekConfig
  <$> choice p [ items := fmap show $ filter (/= Bughouse) $ enumFrom (minBound :: Category) 
               , selection := fromEnum $ seekConfig L.^. scCategory ]
  <*> choice p []
  <*> choice p [ tooltip := "scColor"
               , sorted := False
               , items := fmap show $ enumFrom (minBound :: SeekColor)
               , selection := fromEnum $ seekConfig L.^. scColor ]
  <*> textEntry p [ text := show $ seekConfig L.^. scTime]
  <*> textEntry p [ text := show $ seekConfig L.^. scInc]
  <*> checkBox p [ enabled := not isGuest
                 , checked := (not isGuest) && seekConfig L.^. scRated ]
  <*> checkBox p [ checked := seekConfig L.^. scManual ]
  <*> textEntry p [ text := show $ seekConfig L.^. scRatingFrom ]
  <*> textEntry p [ text := show $ seekConfig L.^. scRatingTo ]

onSelectGameTypeCategory :: WxSeekConfig -> Choice () -> IO ()
onSelectGameTypeCategory wxSeek scCategoryChoice= do
  category <- toEnum `fmap` get scCategoryChoice selection
  set (wxSeek L.^. scBoard)  [ items := fmap displayBoard $ gameTypes Map.! category]

