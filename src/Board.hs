module Board (
  Board (..),
  createBoard
) where

import Api
import PositionParser


import Graphics.UI.WX
import Graphics.UI.WXCore (dcSetUserScale, windowOnMouse)
import Graphics.UI.WXCore.Events (windowOnSize)

import Control.Applicative (liftA)
import Control.Concurrent.STM.TVar
import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)


data Board = Board { _panel :: Panel ()
                   , invertColor :: IO Api.Color
                   , setPosition :: Position -> IO ()}

data BoardState = BoardState { _position :: Position
                             , color :: Api.Color
                             , selSquare :: Square
                             , draggedPiece :: Maybe DraggedPiece
                             , isReadOnly :: Bool
                             } deriving (Show)


data DraggedPiece = DraggedPiece { _point :: Point
                                 , _piece :: Piece
                                 , _square :: Square } deriving (Show)


createBoard :: Panel () -> Position -> IO Board
createBoard p_parent position = do
  let boardState = BoardState position White (Square A One) Nothing True
  vState <- variable [ value := boardState ]
  p_board <- panel p_parent []
  set p_board [ on paint := drawAll p_board vState ]
  windowOnMouse p_board True $ onMouseEvent vState p_board
  let setPosition' p = varGet vState >>= \state -> varSet vState $ state {_position = p}
  return $ Board p_board (invertColor' vState) setPosition'


invertColor' :: Var BoardState -> IO Api.Color
invertColor' vState = do
  state <- varGet vState
  let color' = Api.invert $ Board.color state
  varSet vState $ state {Board.color = color' }
  return color'



drawAll :: Panel () -> Var BoardState -> DC a -> t -> IO ()
drawAll panel vState dc view = do
  state <- varGet vState
  scale <- calcScale `liftA` get panel size
  dcSetUserScale dc scale scale
  drawBoard dc view
  mapM_ (drawPiece dc view (Board.color state)) (_position state)
  if not $ isReadOnly state
    then do
      paintSelectedSquare (selSquare state) (Board.color state) scale dc view
      drawDraggedPiece panel scale (draggedPiece state) dc view
    else return ()



paintSelectedSquare :: Square -> Api.Color -> Double -> DC a -> t -> IO ()
paintSelectedSquare selSq color scale dc view = do
  let pointX' = pointX $ toPos selSq color
      pointY' = pointY $ toPos selSq color
  set dc [penColor := rgb 255 0 (0 :: Int) ]
  drawRect dc (Rect pointX' pointY' 40 40) []



drawDraggedPiece :: Panel () -> Double -> Maybe DraggedPiece -> DC a -> t -> IO ()
drawDraggedPiece p scale mDraggedPiece dc view = do
  case mDraggedPiece of
    Nothing -> return ()
    Just (DraggedPiece pt p _) -> drawBitmap dc (piece p) (point (bar $ pointX pt) (bar $ pointY pt)) True []
    where
      bar pt' = round $ (fromIntegral pt' - 20 * scale) / scale



onMouseEvent :: Var BoardState -> Panel() -> EventMouse -> IO ()
onMouseEvent vState p mouse = do
  state <- varGet vState
  scale <- calcScale `liftA` get p size
  case mouse of
    MouseMotion pt mod -> do
      let square' = toField (scalePoint scale pt) (Board.color state)
      varSet vState $ state {selSquare = square'}
    MouseLeftDown pt mods -> do
      let square' = toField (scalePoint scale pt) (Board.color state)
      let piece = if not $ isReadOnly state then getPiece (_position state) square' (Board.color state) else Nothing
      case piece of
        Just p -> do
          varSet vState state { _position = removePiece (_position state) square'
                              , draggedPiece = Just $ DraggedPiece pt p square'}
        _ -> return ()
    MouseLeftUp click_pt mods -> do
      mDraggedPiece <- draggedPiece `liftA` varGet vState
      case mDraggedPiece of
        Just dp@(DraggedPiece dp_pt piece dp_sq) -> do
          let clicked_sq = toField (scalePoint scale click_pt) (Board.color state)
          let newPosition = movePiece (_position state) clicked_sq dp
          varSet vState state { _position = newPosition, draggedPiece = Nothing}
        _ -> return ()
    MouseLeftDrag pt mode -> do
      let square' = toField (scalePoint scale pt) (Board.color state)
      varSet vState state { selSquare = square', draggedPiece = draggedPiece state >>= setNewPoint pt}
    otherwise -> return ()
  repaint p
  where
    setNewPoint :: Point -> DraggedPiece -> Maybe DraggedPiece
    setNewPoint pt (DraggedPiece pt' p s) = Just $ DraggedPiece pt p s


movePiece :: Position -> Square -> DraggedPiece -> Position
movePiece pos sq (DraggedPiece _ dp_piece dp_sq) = foo $ sq `lookup` pos
  where
    foo :: Maybe Piece -> Position
    foo Nothing = pos ++ [(sq, dp_piece)]
    foo (Just (Piece _ c')) = if pColor dp_piece == c' then pos ++ [(dp_sq, dp_piece)]
                                                       else removePiece pos sq ++ [(sq, dp_piece)]


calcScale :: Size -> Double
calcScale (Size x y) = min (fromIntegral y / 320) (fromIntegral x / 320)



drawPiece :: DC a -> t -> Api.Color -> (Square, Piece) -> IO ()
drawPiece dc view color (square, p) = drawBitmap dc (piece p) (toPos square color) True []



scalePoint :: Double -> Point -> Point
scalePoint scale p = point (foo (pointX p) scale) (foo (pointY p) scale)
  where foo x s = max 0 $ min 319 $ floor (fromIntegral x / s)



drawBoard :: DC a -> t -> IO ()
drawBoard dc view = drawBitmap dc board (point 0 0) False []



toField :: Point -> Api.Color -> Square
toField p color = Square (intToCol color (pointX p `div` 40)) (intToRow color (pointY p `div` 40))


toPos :: Square -> Api.Color -> Point
toPos (Square c r) color = point (colToInt color c * 40) (rowToInt color r * 40)


rowToInt :: Api.Color -> Row -> Int
rowToInt White = abs . (7-) . fromEnum
rowToInt Black = fromEnum


colToInt :: Api.Color -> Column -> Int
colToInt White = fromEnum
colToInt Black = abs . (7-) . fromEnum


intToRow :: Api.Color -> Int -> Row
intToRow White = toEnum . abs . (7-)
intToRow Black = toEnum


intToCol :: Api.Color -> Int -> Column
intToCol White = toEnum
intToCol Black = toEnum . abs . (7-)



board :: Bitmap ()
board = bitmap $ root ++ "board_empty.gif"

piece :: Piece -> Bitmap ()
piece (Piece King Black) = tobitmap "bk.gif"
piece (Piece Queen Black) = tobitmap "bq.gif"
piece (Piece Rook Black) = tobitmap "br.gif"
piece (Piece Knight Black) = tobitmap "bn.gif"
piece (Piece Bishop Black) = tobitmap "bb.gif"
piece (Piece Pawn Black) = tobitmap "bp.gif"
piece (Piece King White) = tobitmap "wk.gif"
piece (Piece Queen White) = tobitmap "wq.gif"
piece (Piece Rook White) = tobitmap "wr.gif"
piece (Piece Knight White) = tobitmap "wn.gif"
piece (Piece Bishop White) = tobitmap "wb.gif"
piece (Piece Pawn White) = tobitmap "wp.gif"


tobitmap gif = bitmap $ root ++ gif


root = "/Users/tilmann/Documents/leksah/XChess/gif/"

initBoard = parsePosition "———— ———— ——p—— ———— ———— ——k-K ———— ———q"


