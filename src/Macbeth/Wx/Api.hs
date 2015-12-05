module Macbeth.Wx.Api (
  BoardState(..),
  addPreMove,
  DraggedPiece(..),
  PieceSet(..)
) where

import Macbeth.Api.Api
import Macbeth.Api.Move
import Macbeth.Api.Game

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
                             , pieceSet :: PieceSet }

addPreMove :: TVar BoardState -> PieceMove -> IO ()
addPreMove vState pm = atomically $ modifyTVar vState (\s -> s {preMoves = preMoves s ++ [pm]})

data DraggedPiece = DraggedPiece { _point :: Point
                                 , _piece :: Piece
                                 , _square :: Square } deriving (Show)

data PieceSet = PieceSet { path :: FilePath, display :: String }
