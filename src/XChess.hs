{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
import Seek
import Game
import AppState
import WxObservedGame


import FicsConnection2 (ficsConnection)
import CommandMsg


import Control.Concurrent (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO (Handle, hPutStrLn)

type Position = [(Square, Piece)]

main = start gui

ficsEventId = wxID_HIGHEST + 51


gui = do
    appState <- newMVar $ AppState Map.empty

    -- main frame
    f  <- frame []
    mv <- newEmptyMVar
    h <- ficsConnection $ handler f mv

    -- right panel
    right <- panel f []
    nb <- notebook right []

    -- tab1 : Sought list
    slp <- panel nb []
    sl  <- listCtrl slp [columns := [ ("#", AlignLeft, -1)
                                    , ("handle", AlignLeft, -1)
                                    , ("rating", AlignLeft, -1)
                                    , ("game type", AlignRight, -1)]
                                    ]

    -- tab2 : console
    cp <- panel nb []
    ct <- textCtrlEx cp (wxTE_MULTILINE .+. wxTE_RICH) [font := fontFixed]
    ce <- entry cp []
    set ce [on enterKey := emitCommand ce h]

    -- tab3 : Games list
    glp <- panel nb []
    gl  <- listCtrl glp [columns := [("#", AlignLeft, -1)
                                    , ("player 1", AlignLeft, -1)
                                    , ("rating", AlignLeft, -1)
                                    , ("player 2", AlignLeft, -1)
                                    , ("rating", AlignLeft, -1)
                                    ]
                        ]
    set gl [on listEvent := onGamesListEvent gl h]

    set f [layout := (container right $
                         column 0
                         [ tabs nb
                            [ tab "Sought" $ container slp $ fill $ widget sl
                            , tab "Games" $ container glp $ fill $ widget gl
                            , tab "Console" $ container cp $
                                            ( column 5  [ floatLeft $ expand $ hstretch $ widget ct
                                                        , expand $ hstretch $ widget ce])
                            ]
                         ]
                       )
          , clientSize := sz 800 400
          ]
    registerFicsEvents f (action h mv ct
      (observeGame appState)
      (updateSeekList sl)
      (updateBoard appState)
      (updateGamesList gl)
      (processGameResult appState))
    return ()


handler :: Frame () -> MVar CommandMsg -> CommandMsg -> IO ()
handler f mv cmd = do putMVar mv cmd >>
                        commandEventCreate wxEVT_COMMAND_MENU_SELECTED ficsEventId >>=
                        \ev -> evtHandlerAddPendingEvent f ev


registerFicsEvents :: Frame () -> IO () -> IO ()
registerFicsEvents f action = evtHandlerOnMenuCommand f ficsEventId action


action :: Handle -> MVar CommandMsg -> TextCtrl () ->
  (Move -> IO ()) ->
  ([Seek] -> IO()) ->
  (Move -> IO ()) ->
  ([Game] -> IO ()) ->
  (Int -> IO ()) ->
  IO ()
action h mvar ct observeGame updateSeekList updateBoard updateGamesList processGameResult = takeMVar mvar >>= \cmd -> case cmd of
  SoughtMsg _ soughtList -> updateSeekList soughtList
  ObserveMsg _ move -> observeGame move >> return ()
  GamesMsg _ games-> updateGamesList games
  AcceptMsg move -> appendText ct (show move ++ "\n")
  g@(MatchMsg id) -> appendText ct $ show g ++ "\n"
  MoveMsg move -> updateBoard move >> appendText ct (show move ++ "\n")
  g@(GameResultMsg id _) -> processGameResult id >> appendText ct (show g)
  TextMessage text -> appendText ct (BS.unpack text ++ "\n")
  LoginMessage     -> hPutStrLn h "guest"
  PasswordMessage  -> hPutStrLn h "efgeon"
  LoggedInMessage  -> hPutStrLn h "set seek 0" >>
                      hPutStrLn h "set style 12" >>
                      hPutStrLn h "iset norwap 1" >>
                      hPutStrLn h "iset block 1"
  SettingsDoneMsg  -> hPutStrLn h "4 games" >> hPutStrLn h "5 sought"
  _                -> return ()



observeGame :: MVar AppState -> Move -> IO ()
observeGame mvar move = do
  appState <- takeMVar mvar
  let gamesMap = observedGames appState
      gameNumber' = Api.gameId move
  case gameNumber' `Map.lookup` gamesMap of
    Just _ -> return ()
    _ -> do
      f <- frame []
      newGame <- createObservedGame f move
      putMVar mvar $ addNewGame appState gameNumber' newGame
      windowShow f
      return ()


updateBoard :: MVar AppState -> Move -> IO ()
updateBoard mvar move = do
  appState <- takeMVar mvar
  let observedGames' = observedGames appState
      gameNumber' = Api.gameId move
  case gameNumber' `Map.lookup` observedGames' of
    Just game@(ObservedGame _ _ updateGame _) -> do
      updateGame move
      putMVar mvar $ appState { observedGames = Map.insert gameNumber' (game `addMove` move) observedGames' }
    _ -> return ()



processGameResult :: MVar AppState -> Int -> IO ()
processGameResult vAppState id = do
  appState <- takeMVar vAppState
  case id `Map.lookup` (observedGames appState) of
    Just (ObservedGame _ _ _  endGame) -> do
      endGame
      putMVar vAppState $ removeObservedGame appState id
    _ -> return ()



updateSeekList :: ListCtrl() -> [Seek] -> IO ()
updateSeekList l seeks = do set l [items := [[show id, name, show rat, show gt] | (Seek id rat name _ _ _ gt _ _) <- seeks]]



updateGamesList :: ListCtrl() -> [Game] -> IO ()
updateGamesList l games = do set l [items := [[show id, n1, show r1, n2, show r2] | (Game id _ r1 n1 r2 n2 _) <- games]]



onGamesListEvent :: t -> Handle -> EventList -> IO ()
onGamesListEvent list h eventList = case eventList of
  ListItemActivated idx    -> hPutStrLn h $ "4 observe " ++ show idx
  _                        -> return ()



emitCommand :: TextCtrl () -> Handle -> IO ()
emitCommand e h = get e text >>= \command ->
                  set e [text := ""] >>
                  hPutStrLn h command



