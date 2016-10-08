module Macbeth.Wx.Game.BoardState (
  BoardState(..),
  DraggedPiece(..),
  Origin(..),
  PieceMove (..),

  pickUpPieceFromHolding,
  discardDraggedPiece,

  setPromotion,
  togglePromotion,

  pointToSquare,
  movePiece,

  update,
  setResult,
  resize,
  invertPerspective,

  performPreMoves,
  cancelLastPreMove,

  setPieceSet,
  getPieceHolding,

  getSelectedSquare,
  initBoardState
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.Player
import Macbeth.Wx.Game.PieceSet
import Macbeth.Fics.Api.Result

import Control.Monad
import Control.Concurrent.STM
import Data.Maybe
import Data.List
import Graphics.UI.WX hiding (position, update, resize, when)
import Safe
import System.IO


data BoardState = BoardState {
    lastMove :: Move
  , isGameUser :: Bool
  , isWhiteUser :: Bool
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


pickUpPieceFromHolding :: TVar BoardState -> PType -> IO ()
pickUpPieceFromHolding vState p = atomically $ modifyTVar vState
  (\s -> let color' = colorUser (lastMove s)
             isAllowed = isGameUser s && p `elem` getPieceHolding color' s
         in if isAllowed
            then s{draggedPiece = Just $ DraggedPiece (mousePt s) (Piece p color') FromHolding }
            else s)


discardDraggedPiece :: TVar BoardState -> IO ()
discardDraggedPiece vState = atomically $ modifyTVar vState (\s -> s {
    draggedPiece = Nothing
  , _position = movePieces (preMoves s) (position $ lastMove s)})


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
  where
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


diffPosition :: Position -> Position -> [PieceMove]
diffPosition before after =
  let from = before \\ after
      to = after \\ before
  in [PieceMove piece1 s1 s2 | (s1, piece1) <- from, (s2, piece2) <- to, piece1 == piece2, s1 /= s2 ]


addtoHistory :: Move -> MoveModifier -> [Move] -> [Move]
addtoHistory _ (Illegal _) mx = mx
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


cancelLastPreMove :: TVar BoardState -> IO ()
cancelLastPreMove vBoardState = atomically $ modifyTVar vBoardState (\s ->
  let preMoves' = fromMaybe [] $ initMay (preMoves s) in s {
      preMoves = preMoves'
    , _position = movePieces preMoves' (position $ lastMove s)})


performPreMoves :: TVar BoardState -> Handle -> IO ()
performPreMoves vBoardState h = do
  preMoves' <- preMoves `fmap` readTVarIO vBoardState
  unless (null preMoves') $ do
    atomically $ modifyTVar vBoardState (\s -> s {
      isWaiting = False,
      preMoves = tail preMoves'})
    hPutStrLn h $ "6 " ++ show (head preMoves' )


setPieceSet :: TVar BoardState -> PieceSet -> IO ()
setPieceSet vState ps = atomically (modifyTVar vState (\s -> s { pieceSet = ps }))


getPieceHolding :: PColor -> BoardState -> [PType]
getPieceHolding White bs = phW bs
getPieceHolding Black bs = phB bs


getSelectedSquare :: BoardState -> Square
getSelectedSquare state = pointToSquare state (mousePt state)


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

    fieldSize :: BoardState -> Double
    fieldSize bs = scale bs * fromIntegral (psize bs)


movePieces :: [PieceMove] -> Position -> Position
movePieces moves' pos = foldl (flip movePiece) pos moves'


movePiece :: PieceMove -> Position -> Position
movePiece (PieceMove piece from to) position = filter (\(s, _) -> s /= from && s /= to) position ++ [(to, piece)]
movePiece (DropMove piece sq) pos = filter (\(s, _) -> s /= sq) pos ++ [(sq, piece)]


initBoardState :: GameProperties -> Username -> BoardState
initBoardState gameProperties' username' = BoardState {
    lastMove = initMove gameProperties'
  , isGameUser = isGameUser' gameProperties'
  , isWhiteUser = playerW' gameProperties' == username'
  , gameResult = Nothing
  , pieceMove = []
  , moves = []
  , _position = []
  , preMoves = []
  , perspective = White
  , mousePt = Point 0 0
  , promotion = Queen
  , draggedPiece = Nothing
  , isWaiting = True
  , psize = 40
  , scale = 1.0
  , pieceSet = head pieceSets
  , phW = []
  , phB = []}

