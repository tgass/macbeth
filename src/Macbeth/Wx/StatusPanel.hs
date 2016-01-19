{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.StatusPanel (
  createStatusPanel
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.FicsMessage hiding (gameId)
import Macbeth.Fics.Api.Move
import Macbeth.Utils.Utils
import Macbeth.Wx.Api
import Macbeth.Wx.Utils

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (Timer, Column, when)

import Macbeth.Wx.PieceSet
import Macbeth.Utils.Board


createStatusPanel :: Panel () -> PColor -> TVar BoardState -> Int -> Chan FicsMessage -> IO (Panel (), ThreadId)
createStatusPanel p color vBoardState eventId chan' = do
  vCmd <- newEmptyMVar
  chan <- dupChan chan'
  move <- lastMove `fmap` readTVarIO vBoardState

  p_status <- panel p []
  t <- newTVarIO $ remainingTime color move
  st <- staticTextFormatted p_status (formatTime $ remainingTime color move)
  tx <- timer p_status [ interval := 1000
                , on command := updateTime t st
                , enabled := isActive move color]

  p_color <- panel p_status [bgcolor := toWxColor color]
  st_playerName <- staticTextFormatted p_status (namePlayer color move)
  p_pieceHoldings <- panel p_status [on paint := paintPieceHolding color vBoardState]

  set p_status [ layout := row 10 [ valignCenter $ minsize (Size 18 18) $ widget p_color
                                  , widget st
                                  , widget st_playerName
                                  , marginWidth 5 $ marginTop $ fill $ widget p_pieceHoldings] ]

  threadId <- forkIO $ eventLoopP eventId chan vCmd p_status
  evtHandlerOnMenuCommand p_status eventId $ takeMVar vCmd >>= \case

    GameMove _ move' -> when (gameId move' == gameId move) $ do
      let time' = remainingTime color move'
      atomically $ swapTVar t time'
      set st [text := formatTime time']
      set tx [enabled := isActive move' color]

    GameResult id _ _ -> when (id == gameId move) $ set tx [enabled := False]

    PieceHolding id phW' phB' -> when (id == gameId move) $ do
      atomically $ modifyTVar vBoardState (\s -> s{ phW = phW', phB = phB' })
      repaint p_status

    _ -> return ()

  return (p_status, threadId)


paintPieceHolding :: PColor -> TVar BoardState -> DC a -> t -> IO ()
paintPieceHolding color state dc _ = do
  px <- getPieceHolding color `fmap` readTVarIO state
  zipWithM_ (drawPiece dc color) [A .. H] (frequency px)
  where
    frequency :: Ord a => [a] -> [(a, Int)]
    frequency = map (head &&& length) . group . sort

drawPiece :: DC a -> PColor -> Column -> (PType, Int) -> IO ()
drawPiece dc color col (ptype, freq) = do
  drawBitmap dc (toBitmap pieceSize (head pieceSets) (Piece ptype color)) (toPos' fieldSize (Square col Eight) White) True []
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
isActive move' color =
  (moveNumber move' /= 1) &&
  (turn move' == color) &&
  (relation move' `elem` [OponentsMove, MyMove, Observing])
