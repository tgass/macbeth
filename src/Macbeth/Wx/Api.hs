module Macbeth.Wx.Api (
  BoardState(..),
  DraggedPiece(..),
  PieceSet(..),
  getPieceHolding,
  addPreMove
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.Game

import Control.Concurrent.STM
import Graphics.UI.WX

data BoardState = BoardState { lastMove :: Move
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
                             , pieceSet :: PieceSet
                             , phW :: [PType]
                             , phB :: [PType] }


data DraggedPiece = DraggedPiece { _point :: Point
                                 , _piece :: Piece
                                 , _square :: Square } deriving (Show)

data PieceSet = PieceSet { path :: FilePath, display :: String }


getPieceHolding :: PColor -> BoardState -> [PType]
getPieceHolding White bs = phW bs
getPieceHolding Black bs = phB bs


addPreMove :: TVar BoardState -> PieceMove -> IO ()
addPreMove vState pm = atomically $ modifyTVar vState (\s -> s {preMoves = preMoves s ++ [pm]})
