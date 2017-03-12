{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.Game.StatusPanel (
  createStatusPanel
) where

import Macbeth.Fics.FicsMessage hiding (gameId, Observing)
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move
import qualified Macbeth.Fics.Api.Result as R
import qualified Macbeth.Wx.RuntimeEnv as E
import Macbeth.Utils.Utils
import Macbeth.Utils.BoardUtils
import Macbeth.Wx.Game.BoardState
import Macbeth.Wx.Game.PieceSet
import Macbeth.Wx.Utils

import Control.Arrow
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Graphics.UI.WX hiding (when)


createStatusPanel :: Panel () -> PColor -> TVar BoardState -> E.RuntimeEnv -> IO (Panel (), FicsMessage -> IO ())
createStatusPanel p color' vBoardState env = do
  lastMove' <- lastMove <$> readTVarIO vBoardState

  p_status <- panel p []
  t <- newTVarIO $ remainingTime color' lastMove'
  st <- staticTextFormatted p_status (formatTime $ remainingTime color' lastMove')
  tx <- timer p_status [ interval := 1000
                , on command := updateTime t st
                , enabled := isActive lastMove' color']

  p_color <- panel p_status [bgcolor := toWxColor color']
  st_playerName <- staticTextFormatted p_status (namePlayer color' lastMove')
  p_pieceHoldings <- panel p_status [on paint := paintPieceHolding color' vBoardState env]

  set p_status [ layout := row 10 [ valignCenter $ minsize (Size 18 18) $ widget p_color
                                  , widget st
                                  , widget st_playerName
                                  , marginWidth 5 $ marginTop $ fill $ widget p_pieceHoldings] ]

  let handler = \case

        GameMove _ move' -> when (gameId move' == gameId lastMove') $ do
          let time' = remainingTime color' move'
          _ <- atomically $ swapTVar t time'
          set st [text := formatTime time']
          set tx [enabled := isActive move' color']

        GameResult result -> when (R.gameId result == gameId lastMove') $ set tx [enabled := False]

        PieceHolding id' phW' phB' -> when (id' == gameId lastMove') $ do
          atomically $ modifyTVar vBoardState (\s -> s{ phW = phW', phB = phB' })
          repaint p_status

        _ -> return ()

  return (p_status, handler)


paintPieceHolding :: PColor -> TVar BoardState -> E.RuntimeEnv -> DC a -> t -> IO ()
paintPieceHolding color' state env dc _ = do
  px <- getPieceHolding color' <$> readTVarIO state
  zipWithM_ (drawPiece dc color') [A .. H] (frequency px)
  where
    frequency :: Ord a => [a] -> [(a, Int)]
    frequency = map (head &&& length) . group . sort


drawPiece :: DC a -> PColor -> Column -> (PType, Int) -> IO ()
drawPiece dc color' col (ptype, freq) = do
  drawBitmap dc (pieceToBitmap pieceSize (head pieceSets) (Piece ptype color'))
                (toPos' fieldSize (Square col Eight) White) True []
  set dc [pen := penColored black 2]
  drawText dc (show freq) (Point (22 + fromEnum col * fieldSize) 15)
    [ fontFace := "Avenir Next Medium"
    , fontSize := 10
    , fontWeight := WeightBold]
  where fieldSize = 35
        pieceSize = 24


updateTime :: TVar Int -> StaticText () -> IO ()
updateTime vTime st = do
  time <- atomically $ modifyTVar vTime (\t -> t - 1) >> readTVar vTime
  set st [text := formatTime time]


isActive :: Move -> PColor -> Bool
isActive move' color' =
  (moveNumber move' /= 1) &&
  (turn move' == color') &&
  (relation move' `elem` [OponentsMove, MyMove, Observing])
