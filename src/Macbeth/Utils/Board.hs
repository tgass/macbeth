module Macbeth.Utils.Board (
  draw,
  onMouseEvent,
  BoardState(..)
) where

import Macbeth.Api.Api

import Paths_Macbeth

import Control.Applicative (liftA)
import Graphics.UI.WX
import Graphics.UI.WXCore (dcSetUserScale)
import System.IO
import System.IO.Unsafe


data BoardState = BoardState { _panel :: Panel()
                             , _position :: Position
                             , playerColor :: Macbeth.Api.Api.PColor
                             , perspective :: Macbeth.Api.Api.PColor
                             , selSquare :: Square
                             , draggedPiece :: Maybe DraggedPiece
                             , isInteractive :: Bool
                             } deriving (Show)


data DraggedPiece = DraggedPiece { _point :: Point
                                 , _piece :: Piece
                                 , _square :: Square } deriving (Show)


draw :: Var BoardState -> DC a -> t -> IO ()
draw vState dc _ = do
  state <- varGet vState
  scale <- calcScale `liftA` get (_panel state) size
  dcSetUserScale dc scale scale
  drawBoard dc
  mapM_ (drawPiece dc (perspective state)) (_position state)
  when (isInteractive state) $ do
    paintSelectedSquare state dc
    drawDraggedPiece scale (draggedPiece state) dc


paintSelectedSquare :: BoardState -> DC a -> IO ()
paintSelectedSquare state dc = do
  let pointX' = pointX $ toPos (selSquare state) (perspective state)
      pointY' = pointY $ toPos (selSquare state) (perspective state)
  set dc [penColor := rgb 255 0 (0 :: Int) ]
  drawRect dc (Rect pointX' pointY' 40 40) []


drawDraggedPiece :: Double -> Maybe DraggedPiece -> DC a -> IO ()
drawDraggedPiece scale mDraggedPiece dc = case mDraggedPiece of
  Nothing -> return ()
  Just (DraggedPiece pt piece _) -> drawBitmap dc (toBitmap piece) (scalePoint pt) True []
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

    _ -> return ()

  repaint (_panel state)

  where
    setNewPoint :: Point -> DraggedPiece -> Maybe DraggedPiece
    setNewPoint pt (DraggedPiece _ p s) = Just $ DraggedPiece pt p s

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


drawPiece :: DC a -> Macbeth.Api.Api.PColor -> (Square, Piece) -> IO ()
drawPiece dc color (square, p) = drawBitmap dc (toBitmap p) (toPos square color) True []


scalePoint :: Double -> Point -> Point
scalePoint scale p = point (foo (pointX p) scale) (foo (pointY p) scale)
  where foo x s = max 0 $ min 319 $ floor (fromIntegral x / s)


drawBoard :: DC a -> IO ()
drawBoard dc = drawBitmap dc board (point 0 0) False []


toPos :: Square -> Macbeth.Api.Api.PColor -> Point
toPos (Square c r) color = point (colToInt color c * 40) (rowToInt color r * 40)


rowToInt :: Macbeth.Api.Api.PColor -> Row -> Int
rowToInt White = abs . (7-) . fromEnum
rowToInt Black = fromEnum


colToInt :: Macbeth.Api.Api.PColor -> Column -> Int
colToInt White = fromEnum
colToInt Black = abs . (7-) . fromEnum


intToRow :: Macbeth.Api.Api.PColor -> Int -> Row
intToRow White = toEnum . abs . (7-)
intToRow Black = toEnum


intToCol :: Macbeth.Api.Api.PColor -> Int -> Column
intToCol White = toEnum
intToCol Black = toEnum . abs . (7-)


board :: Bitmap ()
board = bitmap $ unsafePerformIO $ getDataFileName "board_empty.gif"


pieceToFile :: Piece -> String
pieceToFile (Piece King Black) = "bk.gif"
pieceToFile (Piece Queen Black) = "bq.gif"
pieceToFile (Piece Rook Black) = "br.gif"
pieceToFile (Piece Knight Black) = "bn.gif"
pieceToFile (Piece Bishop Black) = "bb.gif"
pieceToFile (Piece Pawn Black) = "bp.gif"
pieceToFile (Piece King White) = "wk.gif"
pieceToFile (Piece Queen White) = "wq.gif"
pieceToFile (Piece Rook White) = "wr.gif"
pieceToFile (Piece Knight White) = "wn.gif"
pieceToFile (Piece Bishop White) = "wb.gif"
pieceToFile (Piece Pawn White) = "wp.gif"


toBitmap :: Piece -> Bitmap ()
toBitmap p = bitmap $ unsafePerformIO getDataDir ++ pieceToFile p

