{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.Game.StatusPanel (
  wxStatusPanel
) where

import           Control.Arrow
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List
import           Graphics.UI.WX hiding (when, position, color)
import           Macbeth.Fics.Message hiding (gameId, Observing)
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Move
import           Macbeth.Fics.Api.Game
import qualified Macbeth.Fics.Api.Result as R
import           Macbeth.Utils.Utils
import           Macbeth.Utils.BoardUtils hiding (toWxColor)
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
  boardState <- readTVarIO vBoardState
  zipWithM_ (drawPiece (runtimeEnv boardState) dc) [A .. H] (assemblePiecesToShow color boardState)


assemblePiecesToShow :: PColor -> BoardState -> [(Piece, Int)]
assemblePiecesToShow color state
  | isGameWithPH $ gameParams state = frequency $ getPieceHolding color state
  | not $ showCapturedPieces $ boardConfig state = []
  | otherwise = frequency $ capturedPiecesWithColor (invert color) (position $ lastMove state)

  where
    frequency :: Ord a => [a] -> [(a, Int)]
    frequency = map (head &&& length) . group . sort


drawPiece :: RuntimeEnv -> DC a -> Column -> (Piece, Int) -> IO ()
drawPiece runtimeEnv dc col (Piece ptype color, freq) = do
  drawBitmap dc (pieceToBitmap runtimeEnv Alpha1 (Piece ptype color) pieceSize)
                (toPos' fieldSize (Square col Eight) White) True []
  set dc [pen := penColored black 2]
  drawText dc (show freq) (Point (22 + fromEnum col * fieldSize) 15)
    [ fontFace := "Avenir Next Medium"
    , fontSize := 10
    , fontWeight := WeightBold
    ]
  where fieldSize = 35
        pieceSize = 24


updateTime :: TVar Int -> StaticText () -> IO ()
updateTime timeVar st = do
  time <- atomically $ modifyTVar timeVar pred >> readTVar timeVar
  set st [text := formatTime time]


isActive :: Move -> PColor -> Bool
isActive move' color =
  (moveNumber move' /= 1) &&
  (turn move' == color) &&
  (relation move' `elem` [OponentsMove, MyMove, Observing])
