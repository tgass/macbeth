module Macbeth.Utils.Board (
  draw,
  onMouseEvent,
  initBoardState,
  invertPerspective,
  zoomBoard,
  PieceMove (..),
  BoardState(..),
  Zoom(..)
) where

import Macbeth.Api.Api
import Macbeth.Api.Move
import Macbeth.Api.Game
import Macbeth.Wx.Utils
import Paths_Macbeth

import Control.Applicative
import Data.Maybe
import Control.Concurrent.STM
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Row, Column)
import Safe
import System.FilePath
import System.IO
import System.IO.Unsafe


data BoardState = BoardState { _panel :: Panel()
                             , lastMove :: Move
                             , gameResult :: Maybe GameResult
                             , moves :: [Move]
                             , zoom :: Zoom
                             , _position :: Position
                             , preMoves :: [PieceMove]
                             , perspective :: Macbeth.Api.Api.PColor
                             , selSquare :: Square
                             , draggedPiece :: Maybe DraggedPiece
                             , isWaiting :: Bool
                             , psize :: Int
                             }

data Zoom = Small | Medium | Large

data DraggedPiece = DraggedPiece { _point :: Point
                                 , _piece :: Piece
                                 , _square :: Square } deriving (Show)

initBoardState panel move = BoardState {
      _panel = panel
    , lastMove = move
    , gameResult = Nothing
    , moves = [move | isJust $ movePretty move]
    , zoom = Small
    , _position = Macbeth.Api.Move.position move
    , preMoves = []
    , perspective = if relation move == Observing then White else colorUser move
    , selSquare = Square A One
    , draggedPiece = Nothing
    , isWaiting = relation move == MyMove
    , psize = 40}

invertPerspective ::  TVar BoardState -> IO ()
invertPerspective vState = atomically $ modifyTVar vState (\s -> s{perspective = invert $ perspective s})

zoomBoard :: TVar BoardState -> Zoom -> IO ()
zoomBoard vBoardState z = atomically $ modifyTVar vBoardState (\s -> s{zoom = z})

draw :: Var BoardState -> DC a -> t -> IO ()
draw vState dc _ = do
  state <- readTVarIO vState
  scale <- calcScale `liftA` get (_panel state) size
  state <- atomically (modifyTVar vState (\state ->
    state { psize = findSize $ round (40 * scale) :: Int}) >> readTVar vState)
  let scale2 = 40 * scale / fromIntegral (psize state)
  dcSetUserScale dc scale scale
  drawBoard dc
  dcSetUserScale dc scale2 scale2
  when (isHighlightMove $ lastMove state) $ highlightLastMove dc state
  mapM_ (highlightPreMove dc state) (preMoves state)
  mapM_ (drawPiece dc state) (_position state)
  when (isGameUser $ lastMove state) $ paintSelectedSquare dc state
  drawDraggedPiece dc state scale2 (draggedPiece state)


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


paintSquare :: DC a -> Int -> PColor -> Square -> IO ()
paintSquare dc size perspective sq = drawRect dc (squareToRect' size sq perspective) []


calcScale :: Size -> Double
calcScale (Size x y) = min (fromIntegral y / 320) (fromIntegral x / 320)


drawPiece :: DC a -> BoardState -> (Square, Piece) -> IO ()
drawPiece dc state (square, p) = drawBitmap dc (toBitmap size p) (toPos' size square color) True []
  where size = psize state
        color = perspective state


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

pieceToFileName p = pieceToFile p ++ ".png"

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
toBitmap size p = bitmap $ unsafePerformIO getDataDir </> "alpha.ead-01" </> show size </> pieceToFileName p

highlightLastMove :: DC a -> BoardState -> IO ()
highlightLastMove dc state = let
  turn' = turn $ lastMove state
  move' =  (fromJust $ moveVerbose $ lastMove state)
  in sequence_ $ paintHighlight dc state blue `fmap` convertMoveDt turn' move'


highlightPreMove :: DC a -> BoardState -> PieceMove -> IO ()
highlightPreMove dc state preMove = paintHighlight dc state yellow (from preMove, to preMove)

paintHighlight :: DC a -> BoardState -> Color -> (Square, Square) -> IO ()
paintHighlight dc state color (s1, s2) = do
  set dc [penColor := color ]
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

sizes = [20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,52,56,60,64,72,80,88,96,112,128,144,300]

findSize :: Int -> Int
findSize x = fromMaybe 300 $ headMay $ dropWhile (< x) sizes

