{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.StatusPanel (
  createStatusPanel
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.FicsMessage hiding (gameId)
import Macbeth.Fics.Api.Move
import Macbeth.Utils.Utils
import Macbeth.Wx.Utils

import Control.Concurrent
import Control.Concurrent.STM
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Timer)

createStatusPanel :: Panel () -> PColor -> Move -> Int -> Chan FicsMessage -> IO (Panel (), ThreadId)
createStatusPanel p color move eventId chan' = do
  vCmd <- newEmptyMVar
  chan <- dupChan chan'

  p_status <- panel p []
  t <- newTVarIO $ remainingTime color move
  st <- staticTextFormatted p_status (formatTime $ remainingTime color move)
  tx <- timer p_status [ interval := 1000
                , on command := updateTime t st
                , enabled := isActive move color]

  p_color <- panel p_status [bgcolor := toWxColor color]
  st_playerName <- staticTextFormatted p_status (namePlayer color move)

  set p_status [ layout := row 10 [ valignCenter $ minsize (Size 18 18) $ widget p_color
                                  , widget st
                                  , widget st_playerName] ]

  threadId <- forkIO $ eventLoopP eventId chan vCmd p_status
  evtHandlerOnMenuCommand p_status eventId $ takeMVar vCmd >>= \case

    GameMove _ move' -> when (gameId move' == gameId move) $ do
      let time' = remainingTime color move'
      atomically $ swapTVar t time'
      set st [text := formatTime time']
      set tx [enabled := isActive move' color]

    GameResult id _ _ -> when (id == gameId move) $ set tx [enabled := False]

    _ -> return ()

  return (p_status, threadId)

updateTime :: TVar Int -> StaticText () -> IO ()
updateTime vTime st = do
  time <- atomically $ modifyTVar vTime (\t -> t - 1) >> readTVar vTime
  set st [text := formatTime time]

isActive :: Move -> PColor -> Bool
isActive move' color =
  (moveNumber move' /= 1) &&
  (turn move' == color) &&
  (relation move' `elem` [OponentsMove, MyMove, Observing])
