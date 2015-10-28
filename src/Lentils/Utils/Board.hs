module Lentils.Utils.Board (
  draw,
  onMouseEvent,
  BoardState(..)
) where

import Lentils.Api.Api

import Control.Applicative (liftA)
import Control.Monad.Trans.Maybe
import Graphics.UI.WX
import Graphics.UI.WXCore (dcSetUserScale, windowOnMouse)
import System.IO (Handle, hPutStrLn)

data BoardState = BoardState { _panel :: Panel()
                             , _position :: Position
                             , playerColor :: Lentils.Api.Api.PColor
                             , perspective :: Lentils.Api.Api.PColor
                             , selSquare :: Square
                             , draggedPiece :: Maybe DraggedPiece
                             , isInteractive :: Bool
                             } deriving (Show)


data DraggedPiece = DraggedPiece { _point :: Point
                                 , _piece :: Piece
                                 , _square :: Square } deriving (Show)


draw :: Var BoardState -> DC a -> t -> IO ()
draw vState dc view = do
  state <- varGet vState
  scale <- calcScale `liftA` get (_panel state) size
  dcSetUserScale dc scale scale
  drawBoard dc view
  mapM_ (drawPiece dc view (perspective state)) (_position state)
  when (isInteractive state) $ do
    paintSelectedSquare state scale dc view
    drawDraggedPiece scale (draggedPiece state) dc view


paintSelectedSquare :: BoardState -> Double -> DC a -> t -> IO ()
paintSelectedSquare state scale dc view = do
  let pointX' = pointX $ toPos (selSquare state) (perspective state)
      pointY' = pointY $ toPos (selSquare state) (perspective state)
  set dc [penColor := rgb 255 0 (0 :: Int) ]
  drawRect dc (Rect pointX' pointY' 40 40) []


drawDraggedPiece :: Double -> Maybe DraggedPiece -> DC a -> t -> IO ()
drawDraggedPiece scale mDraggedPiece dc view = case mDraggedPiece of
  Nothing -> return ()
  Just (DraggedPiece pt piece _) -> drawBitmap dc (pieceToBitmap piece) (scalePoint pt) True []
  where
    scalePoint pt = point (scaleValue $ pointX pt) (scaleValue $ pointY pt)
    scaleValue value = round $ (fromIntegral value - 20 * scale) / scale


onMouseEvent :: Handle -> Var BoardState -> EventMouse -> IO ()
onMouseEvent h vState evtMouse = do
  state <- varGet vState
  scale <- calcScale `liftA` get (_panel state) size
  let toSquare pt = Square (intToCol (perspective state) (pointX (scalePoint scale pt) `div` 40))
                           (intToRow (perspective state) (pointY (scalePoint scale pt) `div` 40))
  case evtMouse of

    MouseMotion pt _ -> varSet vState $ state {selSquare = toSquare pt}

    MouseLeftDown pt _ -> when (isInteractive state) $ do
        let square' = toSquare pt
        case getPiece (_position state) square' (playerColor state) of
          Just piece -> varSet vState state { _position = removePiece (_position state) square'
                                            , draggedPiece = Just $ DraggedPiece pt piece square'}
          _ -> return ()

    MouseLeftUp click_pt _ -> case draggedPiece state of
      Just dp@(DraggedPiece _ piece dp_sq) -> do
        let clicked_sq = toSquare click_pt
        let newPosition = movePiece (_position state) clicked_sq dp
        varSet vState state { _position = newPosition, draggedPiece = Nothing}
        hPutStrLn h $ "6 " ++ emitMove piece dp_sq clicked_sq
      _ -> return ()

    MouseLeftDrag pt _ -> varSet vState state { selSquare = toSquare pt
                                              , draggedPiece = draggedPiece state >>= setNewPoint pt}

    otherwise -> return ()

  repaint (_panel state)

  where
    setNewPoint :: Point -> DraggedPiece -> Maybe DraggedPiece
    setNewPoint pt (DraggedPiece pt' p s) = Just $ DraggedPiece pt p s

    getPiece :: Position -> Square -> PColor -> Maybe Piece
    getPiece pos sq color = sq `lookup` pos >>= checkColor color
      where
        checkColor :: PColor -> Piece -> Maybe Piece
        checkColor c p@(Piece _ c') = if c == c' then Just p else Nothing


emitMove :: Piece -> Square -> Square -> String
emitMove (Piece King White) (Square E One) (Square G One) = "O-O"
emitMove (Piece King White) (Square E One) (Square C One) = "O-O-O"
emitMove (Piece King Black) (Square E Eight) (Square G Eight) = "O-O"
emitMove (Piece King Black) (Square E Eight) (Square C Eight) = "O-O-O"
emitMove _ s1 s2 = show s1 ++ show s2


movePiece :: Position -> Square -> DraggedPiece -> Position
movePiece pos sq (DraggedPiece _ dp_piece dp_sq) = foo $ sq `lookup` pos
  where
    foo :: Maybe Piece -> Position
    foo Nothing = pos ++ [(sq, dp_piece)]
    foo (Just (Piece _ c')) = if pColor dp_piece == c' then pos ++ [(dp_sq, dp_piece)]
                                                       else removePiece pos sq ++ [(sq, dp_piece)]


removePiece :: Position -> Square -> Position
removePiece pos sq = filter (\(sq', _) -> sq /= sq') pos


calcScale :: Size -> Double
calcScale (Size x y) = min (fromIntegral y / 320) (fromIntegral x / 320)


drawPiece :: DC a -> t -> Lentils.Api.Api.PColor -> (Square, Piece) -> IO ()
drawPiece dc view color (square, p) = drawBitmap dc (pieceToBitmap p) (toPos square color) True []


scalePoint :: Double -> Point -> Point
scalePoint scale p = point (foo (pointX p) scale) (foo (pointY p) scale)
  where foo x s = max 0 $ min 319 $ floor (fromIntegral x / s)


drawBoard :: DC a -> t -> IO ()
drawBoard dc view = drawBitmap dc board (point 0 0) False []


toPos :: Square -> Lentils.Api.Api.PColor -> Point
toPos (Square c r) color = point (colToInt color c * 40) (rowToInt color r * 40)


rowToInt :: Lentils.Api.Api.PColor -> Row -> Int
rowToInt White = abs . (7-) . fromEnum
rowToInt Black = fromEnum


colToInt :: Lentils.Api.Api.PColor -> Column -> Int
colToInt White = fromEnum
colToInt Black = abs . (7-) . fromEnum


intToRow :: Lentils.Api.Api.PColor -> Int -> Row
intToRow White = toEnum . abs . (7-)
intToRow Black = toEnum


intToCol :: Lentils.Api.Api.PColor -> Int -> Column
intToCol White = toEnum
intToCol Black = toEnum . abs . (7-)



board :: Bitmap ()
board = bitmap $ root ++ "board_empty.gif"

pieceToBitmap :: Piece -> Bitmap ()
pieceToBitmap (Piece King Black) = tobitmap "bk.gif"
pieceToBitmap (Piece Queen Black) = tobitmap "bq.gif"
pieceToBitmap (Piece Rook Black) = tobitmap "br.gif"
pieceToBitmap (Piece Knight Black) = tobitmap "bn.gif"
pieceToBitmap (Piece Bishop Black) = tobitmap "bb.gif"
pieceToBitmap (Piece Pawn Black) = tobitmap "bp.gif"
pieceToBitmap (Piece King White) = tobitmap "wk.gif"
pieceToBitmap (Piece Queen White) = tobitmap "wq.gif"
pieceToBitmap (Piece Rook White) = tobitmap "wr.gif"
pieceToBitmap (Piece Knight White) = tobitmap "wn.gif"
pieceToBitmap (Piece Bishop White) = tobitmap "wb.gif"
pieceToBitmap (Piece Pawn White) = tobitmap "wp.gif"


tobitmap gif = bitmap $ root ++ gif


root = "/Users/tilmann/Documents/leksah/XChess/gif/"


