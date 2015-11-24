module Macbeth.Utils.Board (
  draw,
  onMouseEvent,
  initBoardState,
  PieceMove (..),
  BoardState(..)
) where

import Macbeth.Api.Api
import Macbeth.Api.Move
import Macbeth.Wx.Utils
import Paths_Macbeth

import Control.Applicative
import Data.Maybe
import Control.Concurrent.STM
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Row, Column)
import System.IO
import System.IO.Unsafe


data BoardState = BoardState { _panel :: Panel()
                             , lastMove :: Move
                             , _position :: Position
                             , preMoves :: [PieceMove]
                             , perspective :: Macbeth.Api.Api.PColor
                             , selSquare :: Square
                             , draggedPiece :: Maybe DraggedPiece
                             , isWaiting :: Bool
                             }


data DraggedPiece = DraggedPiece { _point :: Point
                                 , _piece :: Piece
                                 , _square :: Square } deriving (Show)

initBoardState panel move = BoardState {
      _panel = panel
    , lastMove = move
    , _position = Macbeth.Api.Move.position move
    , preMoves = []
    , perspective = if relation move == Observing then White else colorUser move
    , selSquare = Square A One
    , draggedPiece = Nothing
    , isWaiting = relation move == MyMove}

draw :: Var BoardState -> DC a -> t -> IO ()
draw vState dc _ = do
  state <- varGet vState
  scale <- calcScale `liftA` get (_panel state) size
  dcSetUserScale dc scale scale
  drawBoard dc
  when (isHighlightMove $ lastMove state) $ highlightLastMove dc state
  mapM_ (highlightPreMove dc (perspective state)) (preMoves state)
  mapM_ (drawPiece dc (perspective state)) (_position state)
  when (isGameUser $ lastMove state) $ paintSelectedSquare dc (perspective state) (selSquare state)
  drawDraggedPiece dc scale (draggedPiece state)


paintSelectedSquare :: DC a -> PColor -> Square -> IO ()
paintSelectedSquare dc perspective sq =
  withBrushStyle brushTransparent $ \transparent -> do
    dcSetBrush dc transparent
    set dc [penColor := red ]
    paintSquare dc perspective sq


drawDraggedPiece :: DC a -> Double -> Maybe DraggedPiece -> IO ()
drawDraggedPiece dc scale mDraggedPiece = case mDraggedPiece of
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

    MouseLeftDown pt _ -> do
        let square' = toSquare pt
        case getPiece (_position state) square' (colorUser $ lastMove state) of
          Just piece -> varSet vState state { _position = removePiece (_position state) square'
                                            , draggedPiece = Just $ DraggedPiece pt piece square'}
          _ -> return ()

    MouseLeftUp click_pt _ -> case draggedPiece state of
      Just (DraggedPiece _ piece dp_sq) -> do
        let clicked_sq = toSquare click_pt
        let newPosition = movePiece (PieceMove piece dp_sq clicked_sq) (_position state)
        varSet vState state { _position = newPosition, draggedPiece = Nothing}
        if isWaiting state
          then hPutStrLn h $ "6 " ++ show (PieceMove piece dp_sq clicked_sq)
          else atomically $ modifyTVar vState (\s -> s {preMoves = preMoves state ++ [PieceMove piece dp_sq clicked_sq]})
      _ -> return ()

    MouseLeftDrag pt _ -> varSet vState state { selSquare = toSquare pt
                                              , draggedPiece = draggedPiece state >>= setNewPoint pt}

    _ -> return ()

  repaint $ _panel state

  where
    removePiece :: Position -> Square -> Position
    removePiece pos sq = filter (\(sq', _) -> sq /= sq') pos

    setNewPoint :: Point -> DraggedPiece -> Maybe DraggedPiece
    setNewPoint pt (DraggedPiece _ p s) = Just $ DraggedPiece pt p s

    getPiece :: Position -> Square -> PColor -> Maybe Piece
    getPiece pos sq color = sq `lookup` pos >>= checkColor color
      where
        checkColor :: PColor -> Piece -> Maybe Piece
        checkColor c p@(Piece _ c') = if c == c' then Just p else Nothing


paintSquare :: DC a -> PColor -> Square -> IO ()
paintSquare dc perspective sq = drawRect dc (squareToRect sq perspective) []


calcScale :: Size -> Double
calcScale (Size x y) = min (fromIntegral y / 320) (fromIntegral x / 320)


drawPiece :: DC a -> PColor -> (Square, Piece) -> IO ()
drawPiece dc color (square, p) = drawBitmap dc (toBitmap p) (toPos square color) True []


scalePoint :: Double -> Point -> Point
scalePoint scale p = point (foo (pointX p) scale) (foo (pointY p) scale)
  where foo x s = max 0 $ min 319 $ floor (fromIntegral x / s)


drawBoard :: DC a -> IO ()
drawBoard dc = drawBitmap dc board (point 0 0) False []


intToRow :: PColor -> Int -> Row
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


highlightLastMove :: DC a -> BoardState -> IO ()
highlightLastMove dc state = let
  perspective' = perspective state
  turn' = turn $ lastMove state
  move' =  (fromJust $ moveVerbose $ lastMove state)
  in sequence_ $ paintHighlight dc perspective' blue `fmap` convertMoveDt turn' move'


highlightPreMove :: DC a -> PColor -> PieceMove -> IO ()
highlightPreMove dc perspective preMove = paintHighlight dc perspective yellow (from preMove, to preMove)

paintHighlight :: DC a -> PColor -> Color -> (Square, Square) -> IO ()
paintHighlight dc perspective color (s1, s2) = do
  set dc [penColor := color ]
  withBrushStyle (BrushStyle (BrushHatch HatchBDiagonal) color) $ \brushBg -> do
    dcSetBrush dc brushBg
    mapM_ (paintSquare dc perspective) [s1, s2]
  withBrushStyle (BrushStyle BrushSolid color) $ \brushArrow -> do
    dcSetBrush dc brushArrow
    drawArrow dc s1 s2 perspective

convertMoveDt _ (Simple s1 s2) = [(s1, s2)]
convertMoveDt Black CastleShort = [ (Square E One, Square G One)
                                  , (Square H One, Square F One)]
convertMoveDt Black CastleLong = [ (Square E One, Square C One)
                                 , (Square A One, Square D One)]
convertMoveDt White CastleShort = [ (Square E Eight, Square G Eight)
                                  , (Square H Eight, Square F Eight)]
convertMoveDt White CastleLong = [ (Square E Eight, Square C Eight)
                                 , (Square A Eight, Square D Eight)]

