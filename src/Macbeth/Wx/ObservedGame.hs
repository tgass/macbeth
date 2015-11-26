module Macbeth.Wx.ObservedGame (
  createObservedGame
) where


import Macbeth.Api.Api
import Macbeth.Api.CommandMsg hiding (gameId)
import Macbeth.Api.Move
import Macbeth.Utils.PGN
import Macbeth.Wx.Utils
import Macbeth.Wx.Clock
import qualified Macbeth.Utils.Board as Board

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Maybe
import Graphics.UI.WX hiding (when, position)
import Graphics.UI.WXCore hiding (when, Timer)
import Safe
import System.IO


eventId = wxID_HIGHEST + 53

createObservedGame :: Handle -> Move -> Chan CommandMsg -> IO ()
createObservedGame h move chan = do
  vCmd <- newEmptyMVar
  vMoves <- newTVarIO [move | isJust $ movePretty move]
  vGameResult <- newMVar Nothing

  f <- frameFixed [ text := frameTitle move ]

  p_back <- panel f []

  -- board
  p_board <- panel p_back []
  let boardState = Board.initBoardState p_board move
  vBoardState <- newTVarIO boardState
  set p_board [ on paint := Board.draw vBoardState ]

  -- player panels
  (p_white, cw) <- createStatusPanel p_back White move
  (p_black, cb) <- createStatusPanel p_back Black move
  let cc = ChessClock cw cb
  updateChessClock move cc

  -- layout helper
  let layoutBoardF = layoutBoard p_board p_white p_black

  -- context menu
  ctxMenu <- menuPane []
  menuItem ctxMenu [ text := "Turn board", on command := turnBoard vBoardState p_back layoutBoardF ]
  when (relation move `elem` [MyMove, OponentsMove]) $ do
                 menuLine ctxMenu
                 menuItem ctxMenu [ text := "Resign", on command := hPutStrLn h "4 resign" ]
                 menuItem ctxMenu [ text := "Offer draw", on command := hPutStrLn h "4 draw" ]
                 menuItem ctxMenu [ text := "Request abort", on command := hPutStrLn h "4 abort"]
                 windowOnMouse p_board True (Board.onMouseEvent h vBoardState)
                 windowOnKeyChar p_board $ onKeysDefault vBoardState p_back

  set p_back [ on clickRight := (\pt -> menuPopup ctxMenu pt p_back)]
  set p_board [ on clickRight := (\pt -> menuPopup ctxMenu pt p_board) ]

  -- status line
  status <- statusField []

  set f [ statusBar := [status]
        , layout := container p_back $ layoutBoardF (Board.perspective boardState)
        ]

  threadId <- forkIO $ eventLoop eventId chan vCmd f
  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd -> case cmd of

    GameMove move' -> when (gameId move' == gameId move) $ do
                                   state <- varGet vBoardState
                                   updateChessClock move' cc
                                   set status [text := ""]
                                   atomically $ modifyTVar vMoves $ addMove move'
                                   updateBoardState vBoardState move'
                                   when (isNextMoveUser move' && not (null $ Board.preMoves state)) $
                                     handlePreMoves vBoardState h
                                   repaint (Board._panel state)

    GameResult id reason result -> when (id == gameId move) $ do
                              windowOnMouse p_board True (\_ -> return ())
                              windowOnKeyChar p_board (\_ -> return ())
                              stopChessClock cc
                              set status [text := (show result ++ " " ++ reason)]
                              swapMVar vGameResult $ Just result
                              hPutStrLn h "4 iset seekinfo 1"
                              killThread threadId

    DrawOffered -> when (isGameUser move) $ do
                     set status [text := nameOponent move ++ " offered a draw. Accept? (y/n)"]
                     keyCommand vBoardState h 'y' "accept" p_back status
                     keyCommand vBoardState h 'n' "decline" p_back status

    AbortRequested user -> when (isGameUser move) $ do
                     set status [text := user ++ " would like to abort the game. Accept? (y/n)"]
                     keyCommand vBoardState h 'y' "abort" p_back status
                     keyCommand vBoardState h 'n' "decline" p_back status

    _ -> return ()

  windowOnDestroy f $ do catch (killThread threadId) (\e -> print $ show (e :: IOException))
                         gameResult <- readMVar vGameResult
                         moves <- readTVarIO vMoves
                         saveAsPGN (reverse moves) gameResult
                         unless (isJust gameResult) $ case relation move of
                           MyMove -> hPutStrLn h "5 resign"
                           OponentsMove -> hPutStrLn h "5 resign"
                           Observing -> hPutStrLn h $ "5 unobserve " ++ show (gameId move)
                           _ -> return ()


keyCommand :: TVar Board.BoardState -> Handle -> Char -> String -> Panel () -> StatusField -> IO ()
keyCommand vBoardState h c command panel status = set panel [on (charKey c) := do
  print ("5 " ++ command)
  hPutStrLn h ("5 " ++ command)
  set status [ text := ""]
  windowOnKeyChar panel $ onKeysDefault vBoardState panel]


onKeysDefault :: TVar Board.BoardState -> Panel () -> EventKey -> IO ()
onKeysDefault vBoardState panel keyboard = case keyKey keyboard of
  KeyChar 'x' -> print "x" >> cancelLastPreMove vBoardState >> repaint panel
  _ -> return ()
  where
    cancelLastPreMove :: TVar Board.BoardState -> IO ()
    cancelLastPreMove vBoardState =
      atomically $ modifyTVar vBoardState (\s ->
        let preMoves' = fromMaybe [] $ initMay (Board.preMoves s) in s {
        Board.preMoves = preMoves'
      , Board._position = movePieces preMoves' (position $ Board.lastMove s)})


handlePreMoves :: TVar Board.BoardState -> Handle -> IO ()
handlePreMoves vBoardState h = do
  preMoves' <- Board.preMoves `fmap` readTVarIO vBoardState
  atomically $ modifyTVar vBoardState (\s -> s {
    Board.isWaiting = False,
    Board.preMoves = tail preMoves'})
  hPutStrLn h $ "6 " ++ show (head preMoves' )


turnBoard :: TVar Board.BoardState -> Panel () -> (PColor -> Layout) -> IO ()
turnBoard vState p layoutF = do
  atomically $ modifyTVar vState (\s -> s{Board.perspective = invert $ Board.perspective s})
  state <- readTVarIO vState
  set p [ layout := layoutF (Board.perspective state) ]
  repaint p


createStatusPanel :: Panel () -> PColor -> Move -> IO (Panel (), Clock)
createStatusPanel p color move = do
  p_status <- panel p []
  cl <- newClock p_status color move
  p_color <- panel p_status [bgcolor := toWxColor color]
  st_playerName <- staticTextFormatted p_status (namePlayer color move)

  set p_status [ layout := row 10 [ valignCenter $ minsize (Size 18 18) $ widget p_color
                                  , widget $ clockTxt cl
                                  , widget st_playerName] ]
  return (p_status, cl)


layoutBoard :: Panel() -> Panel() -> Panel() -> PColor -> Layout
layoutBoard board white black color = column 0 [ hfill $ widget (if color == White then black else white)
                                               , stretch $ minsize (Size 320 320) $ shaped $ widget board
                                               , hfill $ widget (if color == White then white else black)]


{- Add new moves in the front, so I can check for duplicates. -}
addMove :: Move -> [Move] -> [Move]
addMove m [] = [m]
addMove m moves@(m':_)
  | areEqual m m' = moves
  | otherwise = m : moves
    where areEqual m1 m2 = (movePretty m1 == movePretty m2) && (turn m1 == turn m2)


frameTitle move = "[Game " ++ show (gameId move) ++ "] " ++ nameW move ++ " vs " ++ nameB move

updateBoardState vBoardState move =
  atomically $ modifyTVar vBoardState (\s -> s {
    Board.isWaiting = isNextMoveUser move
  , Board.lastMove = move
  , Board._position = movePieces (Board.preMoves s) (position move)})
