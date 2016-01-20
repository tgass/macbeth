module Macbeth.Wx.Board (
  draw,
  onMouseEvent
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move
import Macbeth.Utils.BoardUtils
import Macbeth.Wx.BoardState

import Control.Monad.Reader
import Data.Maybe
import Control.Concurrent.STM
import Graphics.UI.WX hiding (position, update, resize, when)
import Graphics.UI.WXCore hiding (Row, Column, when)
import System.IO

type BoardT a = ReaderT (DC a, BoardState) IO ()

draw :: TVar BoardState -> DC a -> t -> IO ()
draw vState dc _ = do
  state <- readTVarIO vState
  flip runReaderT (dc, state) $ do
    setScale
    drawBoard
    highlightLastMove
    highlightPreMove
    drawPieces
    paintSelectedSquare
    drawDraggedPiece


setScale :: BoardT a
setScale = do
  (dc, state) <- ask
  liftIO $ dcSetUserScale dc (scale state) (scale state)


drawBoard :: BoardT a
drawBoard = do
  (dc, state) <- ask
  let perspective' = perspective state
  let bw = concat $ replicate 4 (concat $ replicate 4 seed ++ replicate 4 (reverse seed))
       where seed = if perspective' == White then [Black, White] else [White, Black]
  let sq = [Square c r  | c <- [A .. H], r <- [One .. Eight]]
  lift $ set dc [ pen := penTransparent ]
  lift $ withBrushStyle (BrushStyle BrushSolid (rgb (180::Int) 150 100)) $ \blackBrush ->
    withBrushStyle (BrushStyle BrushSolid white) $ \whiteBrush ->
      mapM_ (\(c,sq) -> do
        dcSetBrush dc $ if c == White then whiteBrush else blackBrush
        paintSquare dc (psize state) perspective' sq)
          (zip (if perspective' == White then bw else reverse bw) sq)


highlightLastMove :: BoardT a
highlightLastMove = do
  (dc, state) <- ask
  liftIO $ when (isHighlightMove $ lastMove state) $
    sequence_ $ paintHighlight dc state blue `fmap` pieceMove state
  where
    isHighlightMove :: Move -> Bool
    isHighlightMove m = (isJust . moveVerbose) m && (wasOponentMove m || relation m == Observing)


highlightPreMove :: BoardT a
highlightPreMove = do
  (dc, state) <- ask
  liftIO $ sequence_ $ paintHighlight dc state yellow `fmap` preMoves state


drawPieces :: BoardT a
drawPieces = do
  (dc, state) <- ask
  liftIO $ sequence_ $ drawPiece dc state `fmap` _position state
  where
    drawPiece :: DC a -> BoardState -> (Square, Piece) -> IO ()
    drawPiece dc state (sq, p) = drawBitmap dc
      (pieceToBitmap (psize state) (pieceSet state) p)
      (toPos' (psize state) sq (perspective state)) True []


paintSelectedSquare :: BoardT a
paintSelectedSquare = do
  (dc, state) <- ask
  liftIO $ when (isGameUser (lastMove state) && isNothing (gameResult state)) $
    withBrushStyle brushTransparent $ \transparent -> do
      dcSetBrush dc transparent
      set dc [pen := penColored red 1]
      paintSquare dc (psize state) (perspective state) (selSquare state)


drawDraggedPiece :: BoardT a
drawDraggedPiece = do
  (dc, state) <- ask
  case draggedPiece state of
    Nothing -> return ()
    Just (DraggedPiece pt piece _) ->
      liftIO $ drawBitmap dc (pieceToBitmap size (pieceSet state) piece) (scalePoint pt) True []
      where
        scale' = scale state
        size = psize state
        scalePoint pt = point (scaleValue $ pointX pt) (scaleValue $ pointY pt)
        scaleValue value = round $ (fromIntegral value - fromIntegral size / 2 * scale') / scale'


paintHighlight :: DC a -> BoardState -> Color -> PieceMove -> IO ()
paintHighlight dc state color (PieceMove _ s1 s2) = do
  set dc [pen := penColored color 1]
  withBrushStyle (BrushStyle (BrushHatch HatchBDiagonal) color) $ \brushBg -> do
    dcSetBrush dc brushBg
    mapM_ (paintSquare dc (psize state) (perspective state)) [s1, s2]
  withBrushStyle (BrushStyle BrushSolid color) $ \brushArrow -> do
    dcSetBrush dc brushArrow
    drawArrow dc (psize state) s1 s2 (perspective state)


paintSquare :: DC a -> Int -> PColor -> Square -> IO ()
paintSquare dc size perspective sq = drawRect dc (squareToRect' size sq perspective) []


onMouseEvent :: Handle -> Var BoardState -> EventMouse -> IO ()
onMouseEvent h vState evtMouse = do
  state <- varGet vState
  case evtMouse of

    MouseMotion pt _ -> varSet vState $ state {selSquare = pointToSquare state pt}

    MouseLeftDown pt _ -> do
        let square' = pointToSquare state pt
        case getPiece (_position state) square' (colorUser $ lastMove state) of
          Just piece -> varSet vState state { _position = removePiece (_position state) square'
                                            , draggedPiece = Just $ DraggedPiece pt piece square'}
          _ -> return ()

    MouseLeftUp click_pt _ -> case draggedPiece state of
      Just (DraggedPiece _ piece dp_sq) -> do
        let clicked_sq = pointToSquare state click_pt
        let newPosition = movePiece (PieceMove piece dp_sq clicked_sq) (_position state)
        varSet vState state { _position = newPosition, draggedPiece = Nothing}
        if isWaiting state
          then hPutStrLn h $ "6 " ++ show (PieceMove piece dp_sq clicked_sq)
          else addPreMove vState $ PieceMove piece dp_sq clicked_sq
      _ -> return ()

    MouseLeftDrag pt _ -> varSet vState state { selSquare = pointToSquare state pt
                                              , draggedPiece = draggedPiece state >>= setNewPoint pt}

    _ -> return ()


pointToSquare :: BoardState -> Point -> Square
pointToSquare state (Point x y) = Square
  (intToCol (perspective state) (floor $ fromIntegral x / fieldSize state))
  (intToRow (perspective state) (floor $ fromIntegral y / fieldSize state))
  where
    intToRow :: PColor -> Int -> Row
    intToRow White = toEnum . abs . (7-)
    intToRow Black = toEnum

    intToCol :: PColor -> Int -> Column
    intToCol White = toEnum
    intToCol Black = toEnum . abs . (7-)


removePiece :: Position -> Square -> Position
removePiece pos sq = filter (\(sq', _) -> sq /= sq') pos


setNewPoint :: Point -> DraggedPiece -> Maybe DraggedPiece
setNewPoint pt (DraggedPiece _ p s) = Just $ DraggedPiece pt p s


getPiece :: Position -> Square -> PColor -> Maybe Piece
getPiece pos sq color = sq `lookup` pos >>= checkColor color
  where
    checkColor :: PColor -> Piece -> Maybe Piece
    checkColor c p@(Piece _ c') = if c == c' then Just p else Nothing
