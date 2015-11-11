module Macbeth.Utils.Board (
  draw,
  onMouseEvent,
  initBoardState,
  BoardState(..)
) where

import Macbeth.Api.Api
import Macbeth.Api.Move
import Macbeth.Wx.Utils
import Paths_Macbeth

import Control.Applicative
import Data.Maybe
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Row, Column)
import System.IO
import System.IO.Unsafe


data BoardState = BoardState { _panel :: Panel()
                             , lastMove :: Maybe (Square, Square)
                             , _position :: Position
                             , playerColor :: Macbeth.Api.Api.PColor
                             , perspective :: Macbeth.Api.Api.PColor
                             , selSquare :: Square
                             , draggedPiece :: Maybe DraggedPiece
                             , isInteractive :: Bool
                             }


data DraggedPiece = DraggedPiece { _point :: Point
                                 , _piece :: Piece
                                 , _square :: Square } deriving (Show)

initBoardState panel move = BoardState {
      _panel = panel
    , lastMove = moveVerbose move
    , _position = Macbeth.Api.Move.position move
    , Macbeth.Utils.Board.playerColor = colorUser move
    , perspective = if relation move == Observing then White else colorUser move
    , selSquare = Square A One
    , draggedPiece = Nothing
    , isInteractive = relation move == MyMove}

draw :: Var BoardState -> DC a -> t -> IO ()
draw vState dc _ = do
  state <- varGet vState
  scale <- calcScale `liftA` get (_panel state) size
  dcSetUserScale dc scale scale
  drawBoard dc
  when(isJust $ lastMove state) $ highlightLastMove dc (fromJust $ lastMove state) (perspective state)
  mapM_ (drawPiece dc (perspective state)) (_position state)
  when (isInteractive state) $ do
    set dc [penColor := rgb 255 0 (0 :: Int) ]
    paintSquare (selSquare state) (perspective state) dc
    drawDraggedPiece scale (draggedPiece state) dc


paintSquare :: Square -> PColor -> DC a -> IO ()
paintSquare sq color dc = drawRect dc (squareToRect sq color) []


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
        case getPiece (_position state) square' (Macbeth.Utils.Board.playerColor state) of
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


highlightLastMove :: DC a -> (Square, Square) -> PColor -> IO ()
highlightLastMove dc (s1, s2) perspective = do
  set dc [penColor := blue ]
  withBrushStyle (BrushStyle (BrushHatch HatchBDiagonal) blue) $ \brushBg -> do
    dcSetBrush dc brushBg
    paintSquare s1 perspective dc
    paintSquare s2 perspective dc
  withBrushStyle (BrushStyle BrushSolid blue) $ \brushArrow -> do
    dcSetBrush dc brushArrow
    drawArrow dc s1 s2 perspective

