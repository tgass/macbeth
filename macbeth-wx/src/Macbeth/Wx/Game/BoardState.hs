module Macbeth.Wx.Game.BoardState where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import           Data.List
import           Graphics.UI.WX hiding (position, update, resize, when)
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Game
import           Macbeth.Fics.Api.Move
import           Macbeth.Fics.Api.Player
import           Macbeth.Fics.Api.Result
import           Macbeth.Wx.Game.PieceSet (PieceSet(..))
import qualified Macbeth.Wx.Game.PieceSet as PieceSet
import           Macbeth.Wx.Config.BoardConfig
import           Macbeth.Wx.RuntimeEnv
import           Safe
import           System.IO


data BoardState = BoardState {
    gameParams''' :: GameParams
  , lastMove :: Move
  , isGameUser :: Bool
  , userColor_ :: Maybe PColor
  , gameResult :: Maybe GameResult
  , pieceMove :: [PieceMove]
  , history :: [Move]
  , virtualPosition :: Position
  , preMoves :: [PieceMove]
  , perspective :: PColor
  , mousePt :: Point
  , promotion :: PType
  , draggedPiece :: Maybe DraggedPiece
  , isWaiting :: Bool -- macbeth waiting for the user to make a move
  , squareSizePx :: Int
  , pieceImgSize :: Int
  , pieceScale :: Double
  , phW :: [Piece]
  , phB :: [Piece]
  , boardConfig :: BoardConfig
  , runtimeEnv :: RuntimeEnv
  }


data DraggedPiece = DraggedPiece Point Piece PieceSource deriving (Show)

data PieceSource = FromHolding | FromBoard Square deriving (Show)

sourceSquare :: PieceSource -> Maybe Square
sourceSquare FromHolding = Nothing
sourceSquare (FromBoard sq) = Just sq

data PieceMove = PieceMove { piece :: Piece, from :: Square, to :: Square } | DropMove Piece Square

instance Show PieceMove where
  show (PieceMove _ s1 s2) = show s1 ++ show s2
  show (DropMove (Piece p _) s) = show p ++ "@" ++ show s

updateMousePosition :: TVar BoardState -> Point -> IO ()
updateMousePosition vState pt' = atomically $ modifyTVar vState
  (\s -> s{ mousePt = pt', draggedPiece = setNewPoint <$> draggedPiece s})
  where
    setNewPoint :: DraggedPiece -> DraggedPiece
    setNewPoint (DraggedPiece _ p o) = DraggedPiece pt' p o


dropDraggedPiece :: TVar BoardState -> Handle -> Point -> IO ()
dropDraggedPiece vState h click_pt = do
  state <- readTVarIO vState
  void $ runMaybeT $ do
      dp <- MaybeT $ return $ draggedPiece state
      case pointToSquare state click_pt >>= toPieceMove dp of
        Just pieceMove' -> do
          let newPosition = movePiece pieceMove' (virtualPosition state)
          liftIO $ do
              atomically $ modifyTVar vState $ \s -> s{ virtualPosition = newPosition, draggedPiece = Nothing}
              if isWaiting state
                then do
                   atomically $ modifyTVar vState $ \s -> s { isWaiting = False }
                   hPutStrLn h $ "6 " ++ show pieceMove'
                else addPreMove pieceMove'
        Nothing -> liftIO $ discardDraggedPiece vState

  where
    toPieceMove :: DraggedPiece -> Square -> Maybe PieceMove
    toPieceMove (DraggedPiece _ piece' FromHolding) toSq = Just $ DropMove piece' toSq
    toPieceMove (DraggedPiece _ piece' (FromBoard fromSq)) toSq 
      | fromSq /= toSq = Just $ PieceMove piece' fromSq toSq
      | otherwise = Nothing

    addPreMove :: PieceMove -> IO ()
    addPreMove pm = atomically $ modifyTVar vState (\s -> s {preMoves = preMoves s ++ [pm]})


pickUpPieceFromBoard :: TVar BoardState -> Point -> IO ()
pickUpPieceFromBoard vState pt' =
  atomically $ modifyTVar vState (\state' -> fromMaybe state' $ do
    sq' <- pointToSquare state' pt'
    color' <- userColor_ state'
    piece' <- mfilter (hasColor color') (getPiece (virtualPosition state') sq')
    return state' { virtualPosition = removePiece (virtualPosition state') sq'
                  , draggedPiece = Just $ DraggedPiece pt' piece' $ FromBoard sq'})


pickUpPieceFromHolding :: TVar BoardState -> PType -> IO ()
pickUpPieceFromHolding vState p = atomically $ modifyTVar vState
  (\state -> maybe state (pickUpPieceFromHolding' state) (userColor_ state))
  where
    pickUpPieceFromHolding' :: BoardState -> PColor -> BoardState
    pickUpPieceFromHolding' state color'
      | Piece p color'  `elem` getPieceHolding color' state = state { draggedPiece = Just $ DraggedPiece (mousePt state) (Piece p color') FromHolding }
      | otherwise = state


discardDraggedPiece :: TVar BoardState -> IO ()
discardDraggedPiece vState = atomically $ modifyTVar vState (
  \state -> maybe state (discardDraggedPiece' state) (draggedPiece state))
  where
    discardDraggedPiece' :: BoardState -> DraggedPiece -> BoardState
    discardDraggedPiece' s (DraggedPiece _ piece' (FromBoard sq')) = s { draggedPiece = Nothing, virtualPosition = (sq', piece') : virtualPosition s}
    discardDraggedPiece' s _ = s { draggedPiece = Nothing }


invertPerspective :: TVar BoardState -> IO ()
invertPerspective vState = atomically $ modifyTVar vState (\s -> s{perspective = invert $ perspective s})


setResult :: TVar BoardState -> GameResult -> IO ()
setResult vState r = atomically $ modifyTVar vState (\s -> s{
     gameResult = Just r
   , virtualPosition = position $ lastMove s
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
update vBoardState move ctx = atomically $ modifyTVar vBoardState (\s ->
  let preMoves' = if ctx == None then preMoves s else [] in
  s { isWaiting = isNextMoveUser move
    , pieceMove = diffPosition (position $ lastMove s) (position move)
    , history = addtoHistory move ctx (history s)
    , lastMove = move
    , preMoves = preMoves'
    , virtualPosition = let preMovePos' = foldl (flip movePiece) (position move) preMoves'
                        in maybe preMovePos' (removeDraggedPiece preMovePos') (draggedPiece s)})


removeDraggedPiece :: Position -> DraggedPiece -> Position
removeDraggedPiece position' (DraggedPiece _ _ (FromBoard sq')) = removePiece position' sq'
removeDraggedPiece position' _ = position'


diffPosition :: Position -> Position -> [PieceMove]
diffPosition before after =
  let from' = before \\ after
      to' = after \\ before
  in [PieceMove piece1 s1 s2 | (s1, piece1) <- from', (s2, piece2) <- to', piece1 == piece2, s1 /= s2 ]


addtoHistory :: Move -> MoveModifier -> [Move] -> [Move]
addtoHistory _ Illegal{} mx = mx
addtoHistory m Takeback{} mx = m : tail (dropWhile (not . equal m) mx)
  where
    equal :: Move -> Move -> Bool
    equal m1 m2 = (moveNumber m1 == moveNumber m2) && (turn m1 == turn m2)
addtoHistory m None mx = m : mx


resize :: TVar BoardState -> Int -> IO ()
resize vState boardSize' = do
  let (psize', scale') = PieceSet.findSize boardSize'
  atomically $ modifyTVar vState (\s -> 
    let boardConfig' = boardConfig s
        squareSizePx' = round $ fromIntegral psize' * scale'
    in s { squareSizePx = squareSizePx', pieceImgSize = psize', boardConfig = boardConfig' { boardSize = boardSize'}})


cancelLastPreMove :: TVar BoardState -> IO ()
cancelLastPreMove vBoardState = atomically $ modifyTVar vBoardState (\s ->
  let preMoves' = fromMaybe [] $ initMay (preMoves s)
  in s { preMoves = preMoves'
       , virtualPosition = foldl (flip movePiece) (position $ lastMove s) preMoves'})


performPreMoves :: TVar BoardState -> Handle -> IO ()
performPreMoves vBoardState h = do
  preMoves' <- preMoves <$> readTVarIO vBoardState
  unless (null preMoves') $ do
    atomically $ modifyTVar vBoardState (\s ->
      s { isWaiting = False
        , preMoves = tail preMoves'})
    hPutStrLn h $ "6 " ++ show (head preMoves' )


setPieceSet :: TVar BoardState -> PieceSet -> IO ()
setPieceSet vState ps = atomically (modifyTVar vState (\s -> 
  let bc = boardConfig s in s { boardConfig = bc { pieceSet = ps }}))


getPieceHolding :: PColor -> BoardState -> [Piece]
getPieceHolding White = phW
getPieceHolding Black = phB


pointToSquare :: BoardState -> Point -> Maybe Square
pointToSquare state (Point x y) = Square
  <$> intToCol (perspective state) (floor $ fromIntegral x / (fromIntegral $ squareSizePx state :: Double))
  <*> intToRow (perspective state) (floor $ fromIntegral y / (fromIntegral $ squareSizePx state :: Double))
  where
    intToRow :: PColor -> Int -> Maybe Row
    intToRow White = toEnumMay . (7-)
    intToRow Black = toEnumMay

    intToCol :: PColor -> Int -> Maybe Column
    intToCol White = toEnumMay
    intToCol Black = toEnumMay . (7-)

movePiece :: PieceMove -> Position -> Position
movePiece (PieceMove piece' from' to') position' = filter (\(s, _) -> s /= from' && s /= to') position' ++ [(to', piece')]
movePiece (DropMove piece' sq) pos = filter (\(s, _) -> s /= sq) pos ++ [(sq, piece')]


showHighlightMove :: BoardState -> Bool
showHighlightMove state =
  let move = lastMove state
  in (isJust $ moveVerbose move) && ((wasOponentMove move && isWaiting state) || relation move == Observing)

showHighlightCheck :: BoardState -> Bool
showHighlightCheck state = ((isCheck $ lastMove state) || (isCheckmate $ lastMove state)) && maybe True (not . isDraggedKing) (draggedPiece state)

isDraggedKing :: DraggedPiece -> Bool
isDraggedKing (DraggedPiece _ (Piece King _) _) = True
isDraggedKing _ = False


initBoardState :: GameId -> GameParams -> Username -> BoardConfig -> RuntimeEnv -> BoardState
initBoardState gameId' gameParams' username' boardConfig' runtimeEnv' = BoardState {
    gameParams''' = gameParams'
  , lastMove = initMove gameId' gameParams'
  , isGameUser = isGameUser' gameParams'
  , userColor_ = userColor gameParams' username'
  , gameResult = Nothing
  , pieceMove = []
  , history = []
  , virtualPosition = []
  , preMoves = []
  , perspective = fromMaybe White (userColor gameParams' username')
  , mousePt = Point 0 0
  , promotion = Queen
  , draggedPiece = Nothing
  , isWaiting = True
  , squareSizePx = squareSizePx'
  , pieceImgSize = pieceImgSize'
  , pieceScale = pieceScale'
  , phW = []
  , phB = []
  , boardConfig = boardConfig'
  , runtimeEnv = runtimeEnv'
  }
  where
    (pieceImgSize', pieceScale') = PieceSet.findSize $ boardSize boardConfig'
    squareSizePx' = round $ fromIntegral pieceImgSize' * pieceScale'
    

