{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.Game.Board (
  draw,
  onMouseEvent
) where


import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Concurrent.STM
import Data.Maybe
import Graphics.UI.WX hiding (position, update, resize, when, pt, size, value, color)
import Graphics.UI.WXCore hiding (Row, Column, when, pt)
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move
import Macbeth.Utils.BoardUtils
import Macbeth.Wx.Config.BoardConfig
import Macbeth.Wx.Game.BoardState
import Macbeth.Wx.RuntimeEnv
import System.IO

type BoardT a = ReaderT (DC a, BoardState) IO ()

draw :: TVar BoardState -> DC a -> t -> IO ()
draw vState dc _ = do
  state <- readTVarIO vState
  flip runReaderT (dc, state) $ do
    drawBoard
    drawHighlightLastMove
    drawHighlightPreMove
    drawPieces
    drawSelectedSquare
    drawDraggedPiece

drawBoard :: BoardT a
drawBoard = do
  (dc, state) <- ask
  let whiteTile' = whiteTile $ boardConfig state
      blackTile' = blackTile $ boardConfig state
      bw = let seed = (concat $ replicate 4 [Black, White]) in seed ++ reverse seed ++ bw
      sq = [Square c r  | c <- [A .. H], r <- [One .. Eight]]
  lift $ set dc [ pen := penTransparent ]
  lift $ forM_ (zip bw sq) (\(c, sq') ->
    if c == White then drawTile dc state whiteTile' sq' else drawTile dc state blackTile' sq')

drawTile :: DC a -> BoardState -> Tile -> Square -> IO ()
drawTile dc state (BitmapTile b) sq' = do
  bitmapSetSize b $ Size (squareSizePx state) (squareSizePx state)
  drawBitmap dc b (toPos' (squareSizePx state) sq' (perspective state)) True []
drawTile dc state (ColorTile c) sq' = dcWithBrushStyle dc (brushSolid c) $ paintSquare dc state sq'


drawHighlightLastMove :: BoardT a
drawHighlightLastMove = do
  (dc, state) <- ask
  liftIO $ when (showHighlightMove state) $ do
    sequence_ $ paintHighlight dc state blue <$> pieceMove state
    when (showHighlightCheck state) $ paintHighlightCheck dc state (rgb 255 69 00) $ kingSq (lastMove state)
  where
    showHighlightMove :: BoardState -> Bool
    showHighlightMove state = 
      let move = lastMove state 
      in (isJust $ moveVerbose move) && ((wasOponentMove move && isWaiting state) || relation move == Observing)

    showHighlightCheck :: BoardState -> Bool
    showHighlightCheck state = (isCheck $ lastMove state) && maybe True (not . isDraggedKing) (draggedPiece state)


drawHighlightPreMove :: BoardT a
drawHighlightPreMove = do
  (dc, state) <- ask
  liftIO $ sequence_ $ paintHighlight dc state yellow <$> preMoves state


drawPieces :: BoardT a
drawPieces = do
  (dc, state) <- ask
  liftIO $ sequence_ $ drawPiece dc state <$> virtualPosition state
  where
    drawPiece :: DC a -> BoardState -> (Square, Piece) -> IO ()
    drawPiece dc state (sq, piece) = do
      let b = pieceToBitmap (runtimeEnv state) (pieceSet $ boardConfig state) piece (pieceImgSize state)
      bitmapSetSize b $ Size (squareSizePx state) (squareSizePx state)
      drawBitmap dc b (toPos' (squareSizePx state) sq (perspective state)) True []


drawSelectedSquare :: BoardT a
drawSelectedSquare = do
  (dc, state) <- ask
  liftIO $ when (isGameUser state && isNothing (gameResult state)) $
    withBrushStyle brushTransparent $ \transparent -> do
      dcSetBrush dc transparent
      set dc [pen := penColored red 1]
      void $ runMaybeT $ do
        square <- MaybeT $ return $ pointToSquare state $ mousePt state
        liftIO $ paintSquare dc state square


drawDraggedPiece :: BoardT a
drawDraggedPiece = do
  (dc, state) <- ask
  case draggedPiece state of
    Just dp -> liftIO $ drawDraggedPiece'' state dc dp
    _ -> return ()


drawDraggedPiece'' :: BoardState -> DC a -> DraggedPiece -> IO ()
drawDraggedPiece'' state dc (DraggedPiece pt piece _) = drawBitmap dc (pieceToBitmap (runtimeEnv state) (pieceSet $ boardConfig state) piece size) scalePoint True []
  where
    scale' = pieceScale state
    size = pieceImgSize state
    scalePoint = point (scaleValue $ pointX pt) (scaleValue $ pointY pt)
    scaleValue value = round $ (fromIntegral value - fromIntegral size / 2 * scale') / scale'


paintHighlight :: DC a -> BoardState -> Color -> PieceMove -> IO ()
paintHighlight dc state color (PieceMove _ s1 s2) = do
  set dc [pen := penColored color 1]
  withBrushStyle (BrushStyle (BrushHatch HatchBDiagonal) color) $ \brushBg -> do
    dcSetBrush dc brushBg
    mapM_ (paintSquare dc state) [s1, s2]
  withBrushStyle (BrushStyle BrushSolid color) $ \brushArrow -> do
    dcSetBrush dc brushArrow
    drawArrow dc (squareSizePx state) s1 s2 (perspective state)
paintHighlight dc state color (DropMove _ s1) = do
  set dc [pen := penColored color 1]
  withBrushStyle (BrushStyle (BrushHatch HatchBDiagonal) color) $ \brushBg -> do
    dcSetBrush dc brushBg
    paintSquare dc state s1

paintHighlightCheck :: DC a -> BoardState -> Color -> Square -> IO ()
paintHighlightCheck dc state color square = do
  set dc [pen := penColored color 1]
  withBrushStyle (BrushStyle BrushTransparent color) $ \brush -> do
    dcSetBrush dc brush
    mapM_ (paintCircle dc state square) [0.97 - x * 0.1 | x <- [0..5]]

paintSquare :: DC a -> BoardState -> Square -> IO ()
paintSquare dc state sq = drawRect dc (squareToRect' (squareSizePx state) sq (perspective state)) []

paintCircle :: DC a -> BoardState -> Square -> Double -> IO ()
paintCircle dc state sq scale = circle dc pt (floor $ scale * fromIntegral (squareSizePx state) / (2 :: Double)) []
  where pt = squareToPoint (squareSizePx state) sq (perspective state)

onMouseEvent :: Handle -> Var BoardState -> EventMouse -> IO ()
onMouseEvent h vState = \case

    MouseMotion pt _ -> updateMousePosition vState pt

    MouseLeftDown pt _ -> do
      dp <- draggedPiece <$> readTVarIO vState
      case dp of
        (Just _) -> dropDraggedPiece vState h pt -- if draggedPiece is from holding
        Nothing -> pickUpPieceFromBoard vState pt

    MouseLeftUp click_pt _ -> dropDraggedPiece vState h click_pt

    MouseLeftDrag pt _ -> updateMousePosition vState pt

    _ -> return ()

