{-# LANGUAGE TypeFamilies #-}
module Macbeth.Wx.BoardState (
  BoardState(..),
  DraggedPiece(..),
  Origin(..),
  PieceMove (..),
  diffPosition,
  movePiece,
  movePieces,
  toPieceMove,
  setPromotion,
  togglePromotion,

  initBoardState,
  invertPerspective,
  update,
  setResult,
  resize,
  addPreMove,
  cancelLastPreMove,
  handlePreMoves,
  setPieceSet,
  getPieceHolding,
  fieldSize,
  pointToSquare,
  getSelectedSquare,
  drawDraggedPiece''
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.Game
import Macbeth.Utils.BoardUtils
import Macbeth.Wx.PieceSet

import Control.Monad
import Control.Concurrent.STM
import Data.Maybe
import Data.List
import Graphics.UI.WX hiding (position, update, resize, when)
import Safe
import System.IO


data BoardState = BoardState {
    lastMove :: Move
  , gameResult :: Maybe GameResult
  , pieceMove :: [PieceMove]
  , moves :: [Move]
  , _position :: Position
  , preMoves :: [PieceMove]
  , perspective :: PColor
  , mousePt :: Point
  , promotion :: PType
  , draggedPiece :: Maybe DraggedPiece
  , isWaiting :: Bool
  , psize :: Int
  , scale :: Double
  , pieceSet :: PieceSet
  , phW :: [PType]
  , phB :: [PType] }


data DraggedPiece = DraggedPiece Point Piece Origin deriving (Show)

data Origin = FromHolding | FromBoard Square deriving (Show)

data PieceMove = PieceMove { piece :: Piece, from :: Square, to :: Square } | DropMove Piece Square

instance Show PieceMove where
  show (PieceMove _ s1 s2) = show s1 ++ show s2
  show (DropMove (Piece p _) s) = show p ++ "@" ++ show s


diffPosition :: Position -> Position -> [PieceMove]
diffPosition before after =
  let from = before \\ after
      to = after \\ before
  in [PieceMove piece1 s1 s2 | (s1, piece1) <- from, (s2, piece2) <- to, piece1 == piece2, s1 /= s2 ]

movePiece :: PieceMove -> Position -> Position
movePiece (PieceMove piece from to) position = filter (\(s, _) -> s /= from && s /= to) position ++ [(to, piece)]
movePiece (DropMove piece sq) pos = filter (\(s, _) -> s /= sq) pos ++ [(sq, piece)]

movePieces :: [PieceMove] -> Position -> Position
movePieces moves pos = foldl (flip movePiece) pos moves

drawDraggedPiece'' state dc (DraggedPiece pt piece _) = drawBitmap dc (pieceToBitmap size (pieceSet state) piece) (scalePoint pt) True []
  where
    scale' = scale state
    size = psize state
    scalePoint pt = point (scaleValue $ pointX pt) (scaleValue $ pointY pt)
    scaleValue value = round $ (fromIntegral value - fromIntegral size / 2 * scale') / scale'


toPieceMove :: Square -> DraggedPiece -> PieceMove
toPieceMove toSq (DraggedPiece _ piece (FromBoard fromSq)) = PieceMove piece fromSq toSq
toPieceMove toSq (DraggedPiece _ piece FromHolding) = DropMove piece toSq


initBoardState move = BoardState {
    lastMove = move
  , gameResult = Nothing
  , pieceMove = []
  , moves = [move] -- ^ accept empty positions, filter when creating pgn
  , _position = Macbeth.Fics.Api.Move.position move
  , preMoves = []
  , perspective = if relation move == Observing then White else colorUser move
  , mousePt = Point 0 0
  , promotion = Queen
  , draggedPiece = Nothing
  , isWaiting = relation move == MyMove
  , psize = 40
  , scale = 1.0
  , pieceSet = head pieceSets
  , phW = []
  , phB = []}


invertPerspective :: TVar BoardState -> IO ()
invertPerspective vState = atomically $ modifyTVar vState (\s -> s{perspective = invert $ perspective s})


setResult :: TVar BoardState -> GameResult -> IO ()
setResult vState r = atomically $ modifyTVar vState (\s -> s{
     gameResult = Just r
   , _position = position $ lastMove s
   , preMoves = []
   , draggedPiece = Nothing})

setPromotion :: PType -> TVar BoardState -> IO ()
setPromotion p vState = atomically $ modifyTVar vState (\s -> s{promotion = p})


togglePromotion :: TVar BoardState -> IO PType
togglePromotion vState = atomically $ do
  modifyTVar vState (\s -> s{promotion = togglePromotion' (promotion s)})
  promotion `fmap` readTVar vState

togglePromotion' :: PType -> PType
togglePromotion' p = let px = [Queen, Rook, Knight, Bishop]
                     in px !! ((fromJust (p `elemIndex` px) + 1) `mod` length px)


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


resize :: Panel () -> TVar BoardState -> IO ()
resize p vState = do
  (Size x _) <- get p size
  let psize' = pieceSetFindSize x
  atomically $ modifyTVar vState (\s -> s { psize = psize', scale = fromIntegral x / 8 / fromIntegral psize'})


addPreMove :: TVar BoardState -> PieceMove -> IO ()
addPreMove vState pm = atomically $ modifyTVar vState (\s -> s {preMoves = preMoves s ++ [pm]})


cancelLastPreMove :: TVar BoardState -> IO ()
cancelLastPreMove vBoardState = atomically $ modifyTVar vBoardState (\s ->
  let preMoves' = fromMaybe [] $ initMay (preMoves s) in s {
      preMoves = preMoves'
    , _position = movePieces preMoves' (position $ lastMove s)})


handlePreMoves :: TVar BoardState -> Handle -> IO ()
handlePreMoves vBoardState h = do
  preMoves' <- preMoves `fmap` readTVarIO vBoardState
  when (not $ null preMoves') $ do
    atomically $ modifyTVar vBoardState (\s -> s {
      isWaiting = False,
      preMoves = tail preMoves'})
    hPutStrLn h $ "6 " ++ show (head preMoves' )


setPieceSet :: TVar BoardState -> PieceSet -> IO ()
setPieceSet vState ps = atomically (modifyTVar vState (\s -> s { pieceSet = ps }))


getPieceHolding :: PColor -> BoardState -> [PType]
getPieceHolding White bs = phW bs
getPieceHolding Black bs = phB bs


fieldSize :: BoardState -> Double
fieldSize bs = scale bs * fromIntegral (psize bs)


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


getSelectedSquare :: BoardState -> Square
getSelectedSquare state = pointToSquare state (mousePt state)
