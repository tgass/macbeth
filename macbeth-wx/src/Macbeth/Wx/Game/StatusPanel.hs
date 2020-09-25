{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.Game.StatusPanel (
  wxStatusPanel
) where

import           Control.Concurrent.STM
import           Control.Monad
import           Graphics.UI.WX hiding (when, position, color, pt)
import           Macbeth.Fics.Message hiding (gameId, Observing)
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Move
import           Macbeth.Fics.Api.Game
import qualified Macbeth.Fics.Api.Result as R
import           Macbeth.Utils.Utils
import           Macbeth.Utils.BoardUtils
import           Macbeth.Wx.Config.BoardConfig
import           Macbeth.Wx.Game.BoardState
import           Macbeth.Wx.Game.PieceSet
import           Macbeth.Wx.Utils 
import           Macbeth.Wx.RuntimeEnv


wxStatusPanel :: Panel () -> PColor -> TVar BoardState ->  IO (Panel (), Message -> IO ())
wxStatusPanel p color vBoardState = do
  initMove <- lastMove <$> readTVarIO vBoardState
  p_status <- panel p []
  timeVar <- newTVarIO $ remainingTime color initMove

  st <- staticText  p_status [ text := formatTime $ remainingTime color initMove, fontSize := 20 ]
  tx <- timer p_status [ interval := 1000
                       , on command := updateTime timeVar st
                       , enabled := isActive initMove color]

  p_color <- panel p_status [bgcolor := toWxColor color]
  st_playerName <- staticText p_status [ text := namePlayer color initMove, fontSize := 20 ]
  p_pieceHoldings <- panel p_status [on paint := paintPieceHolding color vBoardState ]

  set p_status [ layout := row 10 [ margin 1 $ minsize (Size 18 18) $ widget p_color
                                  , widget st
                                  , widget st_playerName
                                  , minsize (Size 30 30) $ fill $ widget p_pieceHoldings] ]

  let handler = \case

        GameMove _ move -> when (gameId move == gameId initMove) $ do
          let remainingTime' = remainingTime color move
          void $ atomically $ swapTVar timeVar remainingTime'
          set st [text := formatTime remainingTime']
          set tx [enabled := isActive move color]

        GameResult result -> when (R.gameId result == gameId initMove) $ set tx [enabled := False]

        _ -> return ()

  return (p_status, handler)

paintPieceHolding :: PColor -> TVar BoardState -> DC a -> t -> IO ()
paintPieceHolding color vBoardState dc _ = do
  state <- readTVarIO vBoardState
  let pieces = assemblePieces color state
  drawPlus dc pieces state
  zipWithM_ (drawPiece state dc) [A .. H] pieces


assemblePieces :: PColor -> BoardState -> [(Piece, Int)]
assemblePieces color state
  | isGameWithPH $ gameParams state = getPieceHolding color state
  | not $ showCapturedPieces $ boardConfig state = []
  | otherwise = getCapturedPiecesDiff color state 

drawPlus :: DC a -> [(Piece, Int)] -> BoardState -> IO ()
drawPlus dc pieces state 
  | (not $ isGameWithPH $ gameParams state) && (showCapturedPieces $ boardConfig state) && (not $ null pieces) = do
      set dc [pen := penColored black 2]
      drawText dc "+" (Point 4 4)
        [ fontFace := "Avenir Next Medium"
        , fontSize := 12
        , fontWeight := WeightBold
        ]
  | otherwise = return ()

drawPiece :: BoardState -> DC a -> Column -> (Piece, Int) -> IO ()
drawPiece state dc col (Piece ptype color, freq) = do
  drawBitmap dc (pieceToBitmap (runtimeEnv state) Alpha1 (Piece ptype color) pieceSize)
                (setOffset state $ toPos' fieldSize (Square col Eight) White) True []
  set dc [pen := penColored black 2]
  drawText dc (show freq) (setOffset state $ Point (21 + fromEnum col * fieldSize) 15)
    [ fontFace := "Avenir Next Medium"
    , fontSize := 10
    , fontWeight := WeightBold
    ]
  where fieldSize = 35
        pieceSize = 24

setOffset :: BoardState -> Point -> Point
setOffset state pt@(Point x y)
  | isGameWithPH $ gameParams state = pt
  | not $ showCapturedPieces $ boardConfig state = pt
  | otherwise = Point (x + 13) y


updateTime :: TVar Int -> StaticText () -> IO ()
updateTime timeVar st = do
  time <- atomically $ modifyTVar timeVar pred >> readTVar timeVar
  set st [text := formatTime time]


isActive :: Move -> PColor -> Bool
isActive move' color =
  (moveNumber move' /= 1) &&
  (turn move' == color) &&
  (relation move' `elem` [OponentsMove, MyMove, Observing])
