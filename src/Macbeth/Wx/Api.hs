module Macbeth.Wx.Api (
  BoardState(..),
  DraggedPiece(..),
  PieceMove (..),
  movePiece,
  movePieces,
  PieceSet(..)
) where

import Macbeth.Api.Api
import Macbeth.Api.Move
import Macbeth.Api.Game

import Graphics.UI.WX

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

data PieceMove = PieceMove { piece :: Piece, from :: Square, to :: Square }

instance Show PieceMove where
  show (PieceMove _ s1 s2) = show s1 ++ show s2

movePiece :: PieceMove -> Position -> Position
movePiece (PieceMove piece from to) position =
  filter (\(s, _) -> s /= from && s /= to) position ++ [(to, piece)]

movePieces :: [PieceMove] -> Position -> Position
movePieces [] pos = pos
movePieces (x:xs) pos = movePieces xs (movePiece x pos)

data PieceSet = PieceSet { path :: FilePath, display :: String }
