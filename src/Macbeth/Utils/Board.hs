module Macbeth.Utils.Board (
  draw,
  onMouseEvent,
  initBoardState,
  invertPerspective,
  PieceMove (..),
  BoardState(..)
) where

import Macbeth.Api.Api
import Macbeth.Api.Move
import Macbeth.Api.Game
import Macbeth.Wx.Utils
import Macbeth.Wx.PieceSet
import Paths_Macbeth

import Control.Applicative
import Data.Maybe
import Control.Concurrent.STM
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Row, Column)
import System.FilePath
import System.IO
import System.IO.Unsafe


data BoardState = BoardState { lastMove :: Move
                             , gameResult :: Maybe GameResult
                             , moves :: [Move]
                             , _position :: Position
                             , preMoves :: [PieceMove]
                             , perspective :: PColor
                             , selSquare :: Square
                             , draggedPiece :: Maybe DraggedPiece
                             , isWaiting :: Bool
                             , psize :: Int }

data DraggedPiece = DraggedPiece { _point :: Point
                                 , _piece :: Piece
                                 , _square :: Square } deriving (Show)

initBoardState move = BoardState {
      lastMove = move
    , gameResult = Nothing
    , moves = [move | isJust $ movePretty move]
    , _position = Macbeth.Api.Move.position move
    , preMoves = []
    , perspective = if relation move == Observing then White else colorUser move
    , selSquare = Square A One
    , draggedPiece = Nothing
    , isWaiting = relation move == MyMove
    , psize = 40}


invertPerspective ::  TVar BoardState -> IO ()
invertPerspective vState = atomically $ modifyTVar vState (\s -> s{perspective = invert $ perspective s})


draw :: Panel () -> Var BoardState -> DC a -> t -> IO ()
draw _panel vState dc _ = do
  (Size x _) <- get _panel size
  state <- atomically $ modifyTVar vState (\s -> s { psize = pieceSetFindSize alpha_ead01 x}) >> readTVar vState
  let scale = fromIntegral x / 8 / fromIntegral (psize state)
  dcSetUserScale dc scale scale
  drawBoard dc state
  when (isHighlightMove $ lastMove state) $ highlightLastMove dc state
  mapM_ (highlightPreMove dc state) (preMoves state)
  mapM_ (drawPiece dc state) (_position state)
  when (isGameUser $ lastMove state) $ paintSelectedSquare dc state
  drawDraggedPiece dc state scale (draggedPiece state)


drawBoard :: DC a -> BoardState -> IO ()
drawBoard dc state = do
  let perspective' = perspective state
  let bw = concat $ replicate 4 (concat $ replicate 4 seed ++ replicate 4 (reverse seed))
       where seed = if perspective' == White then [Black, White] else [White, Black]
  let sq = [Square c r  | c <- [A .. H], r <- [One .. Eight]]
  set dc [ pen := penTransparent ]
  withBrushStyle (BrushStyle BrushSolid lightgrey) $ \blackBrush ->
    withBrushStyle (BrushStyle BrushSolid white) $ \whiteBrush ->
      mapM_ (\(c,sq) -> do
        dcSetBrush dc $ if c == White then whiteBrush else blackBrush
        paintSquare dc (psize state) perspective' sq)
          (zip (if perspective' == White then bw else reverse bw) sq)


highlightLastMove :: DC a -> BoardState -> IO ()
highlightLastMove dc state = let
  turn' = turn $ lastMove state
  move' =  (fromJust $ moveVerbose $ lastMove state)
  in sequence_ $ paintHighlight dc state blue `fmap` convertMoveDt turn' move'


highlightPreMove :: DC a -> BoardState -> PieceMove -> IO ()
highlightPreMove dc state preMove = paintHighlight dc state yellow (from preMove, to preMove)


paintSelectedSquare :: DC a -> BoardState -> IO ()
paintSelectedSquare dc state =
  withBrushStyle brushTransparent $ \transparent -> do
    dcSetBrush dc transparent
    set dc [penColor := red ]
    paintSquare dc (psize state) (perspective state) (selSquare state)


drawDraggedPiece :: DC a -> BoardState -> Double -> Maybe DraggedPiece -> IO ()
drawDraggedPiece dc state scale mDraggedPiece = case mDraggedPiece of
  Nothing -> return ()
  Just (DraggedPiece pt piece _) -> drawBitmap dc (toBitmap size piece) (scalePoint pt) True []
  where
    size = psize state
    scalePoint pt = point (scaleValue $ pointX pt) (scaleValue $ pointY pt)
    scaleValue value = round $ (fromIntegral value - fromIntegral size / 2 * scale) / scale


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
          else atomically $ modifyTVar vState (\s -> s {preMoves = preMoves state ++ [PieceMove piece dp_sq clicked_sq]})
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


drawPiece :: DC a -> BoardState -> (Square, Piece) -> IO ()
drawPiece dc state (square, p) = drawBitmap dc (toBitmap size p) (toPos' size square (perspective state)) True []
  where size = psize state


intToRow :: PColor -> Int -> Row
intToRow White = toEnum . abs . (7-)
intToRow Black = toEnum


intToCol :: Macbeth.Api.Api.PColor -> Int -> Column
intToCol White = toEnum
intToCol Black = toEnum . abs . (7-)


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


toBitmap :: Int -> Piece -> Bitmap ()
toBitmap size p = bitmap $ unsafePerformIO getDataDir </> path alpha_ead01 </> show size </> pieceToFile p ++ ".png"


paintHighlight :: DC a -> BoardState -> Color -> (Square, Square) -> IO ()
paintHighlight dc state color (s1, s2) = do
  set dc [pen := penColored color 1]
  withBrushStyle (BrushStyle (BrushHatch HatchBDiagonal) color) $ \brushBg -> do
    dcSetBrush dc brushBg
    mapM_ (paintSquare dc (psize state) (perspective state)) [s1, s2]
  withBrushStyle (BrushStyle BrushSolid color) $ \brushArrow -> do
    dcSetBrush dc brushArrow
    drawArrow dc (psize state) s1 s2 (perspective state)


convertMoveDt _ (Simple s1 s2) = [(s1, s2)]
convertMoveDt Black CastleShort = [ (Square E One, Square G One)
                                  , (Square H One, Square F One)]
convertMoveDt Black CastleLong = [ (Square E One, Square C One)
                                 , (Square A One, Square D One)]
convertMoveDt White CastleShort = [ (Square E Eight, Square G Eight)
                                  , (Square H Eight, Square F Eight)]
convertMoveDt White CastleLong = [ (Square E Eight, Square C Eight)
                                 , (Square A Eight, Square D Eight)]


