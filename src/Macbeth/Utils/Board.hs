module Macbeth.Utils.Board (
  draw,
  onMouseEvent,
  initBoardState,
  invertPerspective,
  setResult,
  update,
  resize,
  cancelLastPreMove,
  PieceMove (..),
  toBitmap
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.Game
import Macbeth.Wx.Api
import Macbeth.Wx.Utils
import Macbeth.Wx.PieceSet
import Paths

import Control.Applicative
import Data.Maybe
import Control.Concurrent.STM
import Graphics.UI.WX hiding (position, update, resize)
import Graphics.UI.WXCore hiding (Row, Column)
import Safe
import System.FilePath
import System.IO
import System.IO.Unsafe


initBoardState move = BoardState {
      lastMove = move
    , gameResult = Nothing
    , pieceMove = []
    , moves = [move] -- ^ accept empty positions, filter when creating pgn
    , _position = Macbeth.Fics.Api.Move.position move
    , preMoves = []
    , perspective = if relation move == Observing then White else colorUser move
    , selSquare = Square A One
    , draggedPiece = Nothing
    , isWaiting = relation move == MyMove
    , psize = 40
    , scale = 1.0
    , pieceSet = head pieceSets
    , phW = []
    , phB = []}


invertPerspective ::  TVar BoardState -> IO ()
invertPerspective vState = atomically $ modifyTVar vState (\s -> s{perspective = invert $ perspective s})


setResult :: TVar BoardState -> GameResult -> IO ()
setResult vState r = atomically $ modifyTVar vState (\s -> s{
     gameResult = Just r
   , _position = position $ lastMove s
   , preMoves = []
   , draggedPiece = Nothing})


update :: TVar BoardState -> Move -> MoveModifier -> IO ()
update vBoardState move ctx = atomically $ modifyTVar vBoardState (\s -> s {
    isWaiting = isNextMoveUser move
  , pieceMove = diffPosition (position $ lastMove s) (position move)
  , moves = addtoHistory move ctx (moves s)
  , lastMove = move
  , _position = movePieces (preMoves s) (position move)})


addtoHistory :: Move -> MoveModifier -> [Move] -> [Move]
addtoHistory _ Illegal mx = mx
addtoHistory m (Takeback _) mx = m : tail (dropWhile (not . equal m) mx)
  where
    equal :: Move -> Move -> Bool
    equal m1 m2 = (moveNumber m1 == moveNumber m2) && (turn m1 == turn m2)
addtoHistory m _ mx = m : mx


cancelLastPreMove :: TVar BoardState -> IO ()
cancelLastPreMove vBoardState = atomically $ modifyTVar vBoardState (\s ->
  let preMoves' = fromMaybe [] $ initMay (preMoves s) in s {
      preMoves = preMoves'
    , _position = movePieces preMoves' (position $ lastMove s)})


resize :: Panel () -> TVar BoardState -> IO ()
resize p vState = do
  (Size x _) <- get p size
  let psize' = pieceSetFindSize x
  atomically $ modifyTVar vState (\s -> s { psize = psize', scale = fromIntegral x / 8 / fromIntegral psize'})


draw :: Panel () -> TVar BoardState -> DC a -> t -> IO ()
draw _panel vState dc _ = do
  state <- readTVarIO vState
  dcSetUserScale dc (scale state) (scale state)
  drawBoard dc state
  highlightLastMove dc state
  highlightPreMove dc state
  drawPieces dc state
  paintSelectedSquare dc state
  drawDraggedPiece dc state


drawBoard :: DC a -> BoardState -> IO ()
drawBoard dc state = do
  let perspective' = perspective state
  let bw = concat $ replicate 4 (concat $ replicate 4 seed ++ replicate 4 (reverse seed))
       where seed = if perspective' == White then [Black, White] else [White, Black]
  let sq = [Square c r  | c <- [A .. H], r <- [One .. Eight]]
  set dc [ pen := penTransparent ]
  withBrushStyle (BrushStyle BrushSolid (rgb (180::Int) 150 100)) $ \blackBrush ->
    withBrushStyle (BrushStyle BrushSolid white) $ \whiteBrush ->
      mapM_ (\(c,sq) -> do
        dcSetBrush dc $ if c == White then whiteBrush else blackBrush
        paintSquare dc (psize state) perspective' sq)
          (zip (if perspective' == White then bw else reverse bw) sq)


highlightLastMove :: DC a -> BoardState -> IO ()
highlightLastMove dc state = when (isHighlightMove $ lastMove state) $
  sequence_ $ paintHighlight dc state blue `fmap` pieceMove state


highlightPreMove :: DC a -> BoardState -> IO ()
highlightPreMove dc state = sequence_ $ paintHighlight dc state yellow `fmap` preMoves state


drawPieces :: DC a -> BoardState -> IO ()
drawPieces dc state = sequence_ $ drawPiece `fmap` _position state
  where
    drawPiece :: (Square, Piece) -> IO ()
    drawPiece (sq, p) = drawBitmap dc (toBitmap size (pieceSet state) p) (toPos' size sq (perspective state)) True []
    size = psize state


paintSelectedSquare :: DC a -> BoardState -> IO ()
paintSelectedSquare dc state = when (isGameUser (lastMove state) && isNothing (gameResult state)) $
  withBrushStyle brushTransparent $ \transparent -> do
    dcSetBrush dc transparent
    set dc [pen := penColored red 1]
    paintSquare dc (psize state) (perspective state) (selSquare state)


drawDraggedPiece :: DC a -> BoardState -> IO ()
drawDraggedPiece dc state = case draggedPiece state of
  Nothing -> return ()
  Just (DraggedPiece pt piece _) ->
    drawBitmap dc (toBitmap size (pieceSet state) piece) (scalePoint pt) True []
    where
      scale' = scale state
      size = psize state
      scalePoint pt = point (scaleValue $ pointX pt) (scaleValue $ pointY pt)
      scaleValue value = round $ (fromIntegral value - fromIntegral size / 2 * scale') / scale'


onMouseEvent :: Panel () -> Handle -> Var BoardState -> EventMouse -> IO ()
onMouseEvent _panel h vState evtMouse = do
  state <- varGet vState
  scale <- calcScale `liftA` get _panel size
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
          else addPreMove vState $ PieceMove piece dp_sq clicked_sq
      _ -> return ()

    MouseLeftDrag pt _ -> varSet vState state { selSquare = toSquare pt
                                              , draggedPiece = draggedPiece state >>= setNewPoint pt}

    _ -> return ()

  repaint _panel

  where
    removePiece :: Position -> Square -> Position
    removePiece pos sq = filter (\(sq', _) -> sq /= sq') pos

    setNewPoint :: Point -> DraggedPiece -> Maybe DraggedPiece
    setNewPoint pt (DraggedPiece _ p s) = Just $ DraggedPiece pt p s

    scalePoint :: Double -> Point -> Point
    scalePoint scale p = point (foo (pointX p) scale) (foo (pointY p) scale)
      where foo x s = max 0 $ min 319 $ floor (fromIntegral x / s)

    getPiece :: Position -> Square -> PColor -> Maybe Piece
    getPiece pos sq color = sq `lookup` pos >>= checkColor color
      where
        checkColor :: PColor -> Piece -> Maybe Piece
        checkColor c p@(Piece _ c') = if c == c' then Just p else Nothing


paintSquare :: DC a -> Int -> PColor -> Square -> IO ()
paintSquare dc size perspective sq = drawRect dc (squareToRect' size sq perspective) []


calcScale :: Size -> Double
calcScale (Size x y) = min (fromIntegral y / 320) (fromIntegral x / 320)


intToRow :: PColor -> Int -> Row
intToRow White = toEnum . abs . (7-)
intToRow Black = toEnum


intToCol :: Macbeth.Fics.Api.Api.PColor -> Int -> Column
intToCol White = toEnum
intToCol Black = toEnum . abs . (7-)


toBitmap :: Int -> PieceSet -> Piece -> Bitmap ()
toBitmap size pieceSet p = bitmap $ unsafePerformIO $ getDataFileName $ path pieceSet </> show size </> pieceToFile p ++ ".png"
  where
    pieceToFile :: Piece -> String
    pieceToFile (Piece King Black) = "bk"
    pieceToFile (Piece Queen Black) = "bq"
    pieceToFile (Piece Rook Black) = "br"
    pieceToFile (Piece Knight Black) = "bn"
    pieceToFile (Piece Bishop Black) = "bb"
    pieceToFile (Piece Pawn Black) = "bp"
    pieceToFile (Piece King White) = "wk"
    pieceToFile (Piece Queen White) = "wq"
    pieceToFile (Piece Rook White) = "wr"
    pieceToFile (Piece Knight White) = "wn"
    pieceToFile (Piece Bishop White) = "wb"
    pieceToFile (Piece Pawn White) = "wp"


isHighlightMove :: Move -> Bool
isHighlightMove m = (isJust . moveVerbose) m && (wasOponentMove m || relation m == Observing)


paintHighlight :: DC a -> BoardState -> Color -> PieceMove -> IO ()
paintHighlight dc state color (PieceMove _ s1 s2) = do
  set dc [pen := penColored color 1]
  withBrushStyle (BrushStyle (BrushHatch HatchBDiagonal) color) $ \brushBg -> do
    dcSetBrush dc brushBg
    mapM_ (paintSquare dc (psize state) (perspective state)) [s1, s2]
  withBrushStyle (BrushStyle BrushSolid color) $ \brushArrow -> do
    dcSetBrush dc brushArrow
    drawArrow dc (psize state) s1 s2 (perspective state)

