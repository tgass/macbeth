module Macbeth.Wx.Api (
  BoardState(..),
  DraggedPiece(..),
  PieceSet(..)
) where

import Macbeth.Api.Api
import Macbeth.Api.Move
import Macbeth.Api.Game

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

data DraggedPiece = DraggedPiece { _point :: Point
                                 , _piece :: Piece
                                 , _square :: Square } deriving (Show)

data PieceSet = PieceSet { path :: FilePath, display :: String }
