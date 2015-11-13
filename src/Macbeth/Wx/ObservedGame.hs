module Macbeth.Wx.ObservedGame (
  createObservedGame
) where


import Macbeth.Api.Api
import Macbeth.Api.CommandMsg hiding (gameId)
import Macbeth.Api.Move
import Macbeth.Utils.PGN
import Macbeth.Wx.Utils
import Macbeth.Wx.Clock
import Macbeth.Wx.GameMoves
import qualified Macbeth.Utils.Board as Board

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Maybe
import Graphics.UI.WX hiding (when, position)
import Graphics.UI.WXCore hiding (when, Timer)
import System.IO


eventId = wxID_HIGHEST + 53

createObservedGame :: Handle -> Move -> Chan CommandMsg -> IO ()
createObservedGame h move chan = do
  vCmd <- newEmptyMVar
  vMoves <- newTVarIO [move | isJust $ movePretty move]
  vGameResult <- newMVar Nothing

  f <- frameFixed [ text := frameTitle move ]
  sw <- splitterWindow f []
  (p_moves, updateMoves) <- wxGameMoves sw vMoves

  p_back <- panel sw []

  -- board
  p_board <- panel p_back []
  let boardState = Board.initBoardState p_board move
  vBoardState <- newTVarIO boardState
  set p_board [ on paint := Board.draw vBoardState ]
  windowOnMouse p_board True $ Board.onMouseEvent h vBoardState

  -- player panels
  (p_white, cw) <- createStatusPanel p_back White move
  (p_black, cb) <- createStatusPanel p_back Black move
  let cc = ChessClock cw cb
  updateChessClock move cc

  -- layout helper
  let layoutBoardF = layoutBoard p_board p_white p_black

  -- context menu
  ctxMenu <- menuPane []
  menuItem ctxMenu [ text := "Turn board", on command := turnBoard vBoardState f p_back layoutBoardF ]
  when (relation move `elem` [MyMove, OponentsMove]) $ do
                 menuItem ctxMenu [ text := "Offer draw", on command := hPutStrLn h "4 draw" ]
                 menuItem ctxMenu [ text := "Resign", on command := hPutStrLn h "4 resign" ]
                 void $ menuItem ctxMenu [ text := "Abort", on command := hPutStrLn h "4 abort"]


  set p_back [ on clickRight := (\pt -> menuPopup ctxMenu pt p_back)]
  set p_board [ on clickRight := (\pt -> menuPopup ctxMenu pt p_board) ]

  -- status line
  status <- statusField []
  -- layout

  set f [ layout := fill $ vsplit sw 0 0 (fill $ container p_back $ layoutBoardF (Board.perspective boardState))
                                         (widget p_moves)
        , statusBar := [status]]
  refit p_back

  threadId <- forkIO $ eventLoop eventId chan vCmd f
  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd -> case cmd of

    GameMove move' -> when (gameId move' == gameId move) $ do
                                   state <- varGet vBoardState
                                   varSet vBoardState $ updateBoardState state move'
                                   repaint (Board._panel state)
                                   updateChessClock move' cc
                                   set status [text := ""]
                                   atomically $ modifyTVar vMoves $ addMove move'
                                   updateMoves
                                   set f [ layout := row 5 [layoutBoardF (Board.perspective boardState), widget p_moves] ]

    GameResult id reason result -> when (id == gameId move) $ do
                              atomically $ modifyTVar vBoardState (\s -> s{Board.isInteractive = False})
                              stopChessClock cc
                              set status [text := (show result ++ " " ++ reason)]
                              swapMVar vGameResult $ Just result
                              hPutStrLn h "4 iset seekinfo 1"
                              killThread threadId

    DrawOffered -> when (isGameUser move) $
                     set status [text := nameOponent move ++ " offered a draw. Accept? (y/n)"] >>
                     set p_back [on (charKey 'y') := hPutStrLn h "5 accept" >> unsetKeyHandler p_back] >>
                     set p_back [on (charKey 'n') := hPutStrLn h "5 decline" >> unsetKeyHandler p_back]

    DrawDeclined -> when (isGameUser move) $
                      set status [text := nameOponent move ++ " declined your draw offer."]

    AbortRequested user -> when (isGameUser move) $
                     set status [text := user ++ " would like to abort the game. Accept? (y/n)"] >>
                     set p_back [on (charKey 'y') := hPutStrLn h "5 abort" >> unsetKeyHandler p_back] >>
                     set p_back [on (charKey 'n') := hPutStrLn h "5 decline" >> unsetKeyHandler p_back]

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


turnBoard :: TVar Board.BoardState -> Frame () -> Panel () -> (PColor -> Layout) -> IO ()
turnBoard vState f p layoutF = do
  atomically $ modifyTVar vState (\s -> s{Board.perspective = invert $ Board.perspective s})
  state <- readTVarIO vState
  set p [ layout := layoutF (Board.perspective state) ]
  refit f


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
                                               , fill $ minsize (Size 320 320) $ widget board
                                               , hfill $ widget (if color == White then white else black)]


unsetKeyHandler :: Panel () -> IO ()
unsetKeyHandler p = set p [on (charKey 'y') := return ()] >>
                    set p [on (charKey 'n') := return ()]


{- Add new moves in the front, so I can check for duplicates. -}
addMove :: Move -> [Move] -> [Move]
addMove m [] = [m]
addMove m moves@(m':_)
  | areEqual m m' = moves
  | otherwise = m : moves
    where areEqual m1 m2 = (movePretty m1 == movePretty m2) && (turn m1 == turn m2)


frameTitle move = "[Game " ++ show (gameId move) ++ "] " ++ nameW move ++ " vs " ++ nameB move

updateBoardState s m = s { Board._position = position m
                       , Board.isInteractive = isNextMoveUser m
                       , Board.lastMove = m}
