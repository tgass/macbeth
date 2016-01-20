module Macbeth.Wx.BoardState (
  BoardState(..),
  DraggedPiece(..),
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
  fieldSize
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.Game
import Macbeth.Wx.PieceSet

import Control.Monad
import Control.Concurrent.STM
import Data.Maybe
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
  , selSquare :: Square
  , draggedPiece :: Maybe DraggedPiece
  , isWaiting :: Bool
  , psize :: Int
  , scale :: Double
  , pieceSet :: PieceSet
  , phW :: [PType]
  , phB :: [PType] }


data DraggedPiece = DraggedPiece {
    _point :: Point
  , _piece :: Piece
  , _square :: Square } deriving (Show)

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


invertPerspective :: TVar BoardState -> IO ()
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
