module Macbeth.Wx.Utils (
  eventLoop,
  listItemRightClickEvent,
  toWxColor,
  staticTextFormatted,
  drawArrow,
  squareToRect,
  toPos,
  rowToInt,
  colToInt
) where

import Macbeth.Api.Api
import Macbeth.Api.CommandMsg

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Column, Row)


eventLoop :: Int -> Chan CommandMsg -> MVar CommandMsg -> Frame () -> IO ()
eventLoop id chan vCmd f = readChan chan >>= putMVar vCmd >>
  commandEventCreate wxEVT_COMMAND_MENU_SELECTED id >>= evtHandlerAddPendingEvent f >>
  eventLoop id chan vCmd f


listItemRightClickEvent :: ListCtrl a -> (Graphics.UI.WXCore.ListEvent () -> IO ()) -> IO ()
listItemRightClickEvent listCtrl eventHandler
  = windowOnEvent listCtrl [wxEVT_COMMAND_LIST_ITEM_RIGHT_CLICK] eventHandler listHandler
    where
      listHandler :: Graphics.UI.WXCore.Event () -> IO ()
      listHandler evt = eventHandler $ objectCast evt


toWxColor :: Macbeth.Api.Api.PColor -> Graphics.UI.WXCore.Color
toWxColor White = white
toWxColor Black = black

staticTextFormatted :: Panel () -> String -> IO (StaticText ())
staticTextFormatted p s = staticText p [ text := s
                                       , fontFace := "Avenir Next Medium"
                                       , fontSize := 20
                                       , fontWeight := WeightBold]

rowToInt :: Macbeth.Api.Api.PColor -> Row -> Int
rowToInt White = abs . (7-) . fromEnum
rowToInt Black = fromEnum


colToInt :: Macbeth.Api.Api.PColor -> Column -> Int
colToInt White = fromEnum
colToInt Black = abs . (7-) . fromEnum


toPos :: Square -> Macbeth.Api.Api.PColor -> Point
toPos (Square c r) color = point (colToInt color c * 40) (rowToInt color r * 40)


squareToRect :: Square -> PColor -> Rect
squareToRect sq color = Rect pointX' pointY' 40 40
  where pointX' = pointX $ toPos sq color
        pointY' = pointY $ toPos sq color

drawArrow dc s1 s2 perspective = drawArrowPt dc (pt (x1+20) (y1+20)) (pt (x2'+20) (y2'+20))
    where (Rect x1 y1 _ _ ) = squareToRect s1 perspective
          (Rect x2 y2 _ _ ) = squareToRect s2 perspective
          x2'
             | x2 > x1 = x2 - 20
             | x2 < x1 = x2 + 20
             | otherwise = x2
          y2'
             | y2 > y1 = y2 - 20
             | y2 < y1 = y2 + 20
             | otherwise = y2

drawArrowPt dc p1 p2 = do
  line dc p1 p2 []
  polygon dc (fmap (movePt (pointX p2) (pointY p2) . rotate (dfix-90)) triangle) []
  where
    triangle = [(0, 0), (-2, -7), (2, -7)]
    movePt x y (Point px py) = pt (x + px) (y + py)
    deltaX = pointX p2 - pointX p1
    deltaY = pointY p2 - pointY p1
    d = atan(fromIntegral deltaY / fromIntegral deltaX) * 180 / pi
    dfix = if deltaX < 0 then d -180 else d


rotate :: Double -> (Int, Int) -> Point
rotate d ptx = pt (round $ x * cos rad - y * sin rad) (round $ x * sin rad + y * cos rad)
  where x = fromIntegral $ fst ptx
        y = fromIntegral $ snd ptx
        rad = d/180*pi


