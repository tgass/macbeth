module Board (
  Board (..),
  createBoard
) where

import Api

import Control.Applicative (liftA)
import Graphics.UI.WX
import Graphics.UI.WXCore (dcSetUserScale, windowOnMouse)
import System.IO (Handle, hPutStrLn)


data Board = Board { _panel :: Panel ()
                   , invertColor :: IO Api.PColor
                   , setPosition :: Position -> IO ()
                   , setInteractive :: Bool -> IO ()
                   , repaintBoard :: IO ()}

data BoardState = BoardState { _position :: Position
                             , color :: Api.PColor
                             , selSquare :: Square
                             , draggedPiece :: Maybe DraggedPiece
                             , isInteractive :: Bool
                             } deriving (Show)


data DraggedPiece = DraggedPiece { _point :: Point
                                 , _piece :: Piece
                                 , _square :: Square } deriving (Show)


createBoard :: Handle -> Panel () -> Position -> Api.PColor -> Bool -> IO Board
createBoard h p_parent position color interactive = do
  let boardState = BoardState position color (Square A One) Nothing interactive
  vState <- variable [ value := boardState ]
  p_board <- panel p_parent []
  set p_board [ on paint := drawAll p_board vState ]
  windowOnMouse p_board True $ onMouseEvent h vState p_board
  let setPosition' p = do
                         state <- varGet vState
                         varSet vState $ state {_position = p}
  let invertColor' vState = do
                               state <- varGet vState
                               let color' = Api.invert $ Board.color state
                               varSet vState $ state {Board.color = color' }
                               return color'
  let setInteractive' i = do
                           state <- varGet vState
                           varSet vState $ state {isInteractive = i}
  let repaintBoard = repaint p_board
  return $ Board p_board (invertColor' vState) setPosition' setInteractive' repaintBoard



drawAll :: Panel () -> Var BoardState -> DC a -> t -> IO ()
drawAll panel vState dc view = do
  state <- varGet vState
  scale <- calcScale `liftA` get panel size
  dcSetUserScale dc scale scale
  drawBoard dc view
  mapM_ (drawPiece dc view (Board.color state)) (_position state)
  when (isInteractive state) $ do
    paintSelectedSquare (selSquare state) (Board.color state) scale dc view
    drawDraggedPiece scale (draggedPiece state) dc view


paintSelectedSquare :: Square -> Api.PColor -> Double -> DC a -> t -> IO ()
paintSelectedSquare selSq color scale dc view = do
  let pointX' = pointX $ toPos selSq color
      pointY' = pointY $ toPos selSq color
  set dc [penColor := rgb 255 0 (0 :: Int) ]
  drawRect dc (Rect pointX' pointY' 40 40) []



drawDraggedPiece :: Double -> Maybe DraggedPiece -> DC a -> t -> IO ()
drawDraggedPiece scale mDraggedPiece dc view = case mDraggedPiece of
  Nothing -> return ()
  Just (DraggedPiece pt piece _) -> drawBitmap dc (pieceToBitmap piece) (scalePoint pt) True []
  where
    scalePoint pt = point (scaleValue $ pointX pt) (scaleValue $ pointY pt)
    scaleValue value = round $ (fromIntegral value - 20 * scale) / scale



onMouseEvent :: Handle -> Var BoardState -> Panel() -> EventMouse -> IO ()
onMouseEvent h vState p mouse = do
  state <- varGet vState
  scale <- calcScale `liftA` get p size
  case mouse of
    MouseMotion pt mod -> do
      let square' = toField (scalePoint scale pt) (Board.color state)
      varSet vState $ state {selSquare = square'}
    MouseLeftDown pt mods -> do
      let square' = toField (scalePoint scale pt) (Board.color state)
      let piece = if isInteractive state then getPiece (_position state) square' (Board.color state) else Nothing
      case piece of
        Just p -> varSet vState state { _position = removePiece (_position state) square'
                                      , draggedPiece = Just $ DraggedPiece pt p square'}
        _ -> return ()
    MouseLeftUp click_pt mods -> do
      mDraggedPiece <- draggedPiece `liftA` varGet vState
      case mDraggedPiece of
        Just dp@(DraggedPiece dp_pt piece dp_sq) -> do
          let clicked_sq = toField (scalePoint scale click_pt) (Board.color state)
          let newPosition = movePiece (_position state) clicked_sq dp
          varSet vState state { _position = newPosition, draggedPiece = Nothing}
          hPutStrLn h $ "6 " ++ emitMove piece dp_sq clicked_sq
        _ -> return ()
    MouseLeftDrag pt mode -> do
      let square' = toField (scalePoint scale pt) (Board.color state)
      varSet vState state { selSquare = square', draggedPiece = draggedPiece state >>= setNewPoint pt}
    otherwise -> return ()
  repaint p
  where
    setNewPoint :: Point -> DraggedPiece -> Maybe DraggedPiece
    setNewPoint pt (DraggedPiece pt' p s) = Just $ DraggedPiece pt p s


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


calcScale :: Size -> Double
calcScale (Size x y) = min (fromIntegral y / 320) (fromIntegral x / 320)



drawPiece :: DC a -> t -> Api.PColor -> (Square, Piece) -> IO ()
drawPiece dc view color (square, p) = drawBitmap dc (pieceToBitmap p) (toPos square color) True []



scalePoint :: Double -> Point -> Point
scalePoint scale p = point (foo (pointX p) scale) (foo (pointY p) scale)
  where foo x s = max 0 $ min 319 $ floor (fromIntegral x / s)



drawBoard :: DC a -> t -> IO ()
drawBoard dc view = drawBitmap dc board (point 0 0) False []



toField :: Point -> Api.PColor -> Square
toField p color = Square (intToCol color (pointX p `div` 40)) (intToRow color (pointY p `div` 40))


toPos :: Square -> Api.PColor -> Point
toPos (Square c r) color = point (colToInt color c * 40) (rowToInt color r * 40)


rowToInt :: Api.PColor -> Row -> Int
rowToInt White = abs . (7-) . fromEnum
rowToInt Black = fromEnum


colToInt :: Api.PColor -> Column -> Int
colToInt White = fromEnum
colToInt Black = abs . (7-) . fromEnum


intToRow :: Api.PColor -> Int -> Row
intToRow White = toEnum . abs . (7-)
intToRow Black = toEnum


intToCol :: Api.PColor -> Int -> Column
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


