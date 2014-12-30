module AppState (
  AppState (..),
  addNewGame,
  removeObservedGame
) where

import Api
import Seek
import WxObservedGame

import Data.Map as Map


data AppState = AppState { observedGames :: Map Int ObservedGame }


addNewGame :: AppState -> Int -> ObservedGame -> AppState
addNewGame appState idx game = appState { observedGames = Map.insert idx game gamesMap }
  where gamesMap = observedGames appState


removeObservedGame :: AppState -> Int -> AppState
removeObservedGame appState idx = appState { observedGames = Map.delete idx gamesMap }
  where gamesMap = observedGames appState
