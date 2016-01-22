{-# LANGUAGE LambdaCase #-}

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
    drawHighlightLastMove
    drawHighlightPreMove
    drawPieces
    drawSelectedSquare
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
        paintSquare dc state sq)
          (zip (if perspective' == White then bw else reverse bw) sq)


drawHighlightLastMove :: BoardT a
drawHighlightLastMove = do
  (dc, state) <- ask
  liftIO $ when (isHighlightMove $ lastMove state) $
    sequence_ $ paintHighlight dc state blue `fmap` pieceMove state
  where
    isHighlightMove :: Move -> Bool
    isHighlightMove m = (isJust . moveVerbose) m && (wasOponentMove m || relation m == Observing)


drawHighlightPreMove :: BoardT a
drawHighlightPreMove = do
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


drawSelectedSquare :: BoardT a
drawSelectedSquare = do
  (dc, state) <- ask
  liftIO $ when (isGameUser (lastMove state) && isNothing (gameResult state)) $
    withBrushStyle brushTransparent $ \transparent -> do
      dcSetBrush dc transparent
      set dc [pen := penColored red 1]
      paintSquare dc state (getSelectedSquare state)


drawDraggedPiece :: BoardT a
drawDraggedPiece = do
  (dc, state) <- ask
  case draggedPiece state of
    Just dp -> liftIO $ drawDraggedPiece'' state dc dp
    _ -> return ()


drawDraggedPiece'' :: BoardState -> DC a -> DraggedPiece -> IO ()
drawDraggedPiece'' state dc (DraggedPiece pt piece _) = drawBitmap dc (pieceToBitmap size (pieceSet state) piece) (scalePoint pt) True []
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
    mapM_ (paintSquare dc state) [s1, s2]
  withBrushStyle (BrushStyle BrushSolid color) $ \brushArrow -> do
    dcSetBrush dc brushArrow
    drawArrow dc (psize state) s1 s2 (perspective state)
paintHighlight dc state color (DropMove _ s1) = do
  set dc [pen := penColored color 1]
  withBrushStyle (BrushStyle (BrushHatch HatchBDiagonal) color) $ \brushBg -> do
    dcSetBrush dc brushBg
    paintSquare dc state s1


paintSquare :: DC a -> BoardState -> Square -> IO ()
paintSquare dc state sq = drawRect dc (squareToRect' (psize state) sq (perspective state)) []


onMouseEvent :: Handle -> Var BoardState -> EventMouse -> IO ()
onMouseEvent h vState = \case

    MouseMotion pt _ -> updateMousePosition vState pt

    MouseLeftDown pt _ -> do
      dp <- draggedPiece `fmap` readTVarIO vState
      if isJust dp then dropDraggedPiece vState h pt else pickUpPiece vState pt

    MouseLeftUp click_pt _ -> dropDraggedPiece vState h click_pt

    MouseLeftDrag pt _ -> updateMousePosition vState pt

    _ -> return ()

