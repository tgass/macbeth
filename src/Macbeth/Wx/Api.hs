module Macbeth.Wx.Api (
  PieceSet(..),
  BoardState(..),
  DraggedPiece(..)
) where

import Macbeth.Api.Api
import Macbeth.Api.Move
import Macbeth.Api.Game

import Graphics.UI.WX


data PieceSet = PieceSet { path :: FilePath, display :: String }

data BoardState = BoardState { lastMove :: Move
                             , gameResult :: Maybe GameResult
                             , moves :: [Move]
                             , _position :: Position
                             , preMoves :: [PieceMove]
                             , perspective :: PColor
                             , selSquare :: Square
                             , draggedPiece :: Maybe DraggedPiece
                             , isWaiting :: Bool
                             , psize :: Int
                             , pieceSet :: PieceSet }

data DraggedPiece = DraggedPiece { _point :: Point
                                 , _piece :: Piece
                                 , _square :: Square } deriving (Show)

