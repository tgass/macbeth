module Lentils.Wx.ObservedGame (
  createObservedGame,
) where

import Lentils.Api.Api
import Lentils.Api.CommandMsg hiding (gameId)
import Lentils.Api.Move
import Lentils.Utils.PGN
import Lentils.Utils.Utils
import Lentils.Wx.Utils
import qualified Lentils.Utils.Board as Board

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Exception
import Control.Monad
import Data.Maybe
import Graphics.UI.WX hiding (when, position)
import Graphics.UI.WXCore hiding (when)
import System.IO


eventId = wxID_HIGHEST + 53

createObservedGame :: Handle -> Move -> Chan CommandMsg -> IO ()
createObservedGame h move chan = do
  let color = if relation move == Observing then White else colorUser move
  vCmd <- newEmptyMVar
  vMoves <- newMVar [move | isJust $ movePretty move]
  vGameResult <- newMVar Nothing

  f <- frame [ text := frameTitle move ]
  p_back <- panel f []

  -- board
  p_board <- panel p_back []
  vBoardState <- variable [ value := Board.BoardState p_board (position move) color color (Square A One) Nothing (relation move == MyMove) ]
  set p_board [ on paint := Board.draw vBoardState ]
  windowOnMouse p_board True $ Board.onMouseEvent h vBoardState

  -- clock
  vClock <- variable [ value := move ]

  -- status line
  status <- statusField []

  -- panels
  (p_white, t_white) <- createStatusPanel p_back vClock White
  (p_black, t_black) <- createStatusPanel p_back vClock Black

  -- layout helper
  let layoutBoardF = layoutBoard p_board p_white p_black

  -- context menu
  ctxMenu <- menuPane []
  menuItem ctxMenu [ text := "Turn board", on command := turnBoard vBoardState p_back layoutBoardF ]
  when (relation move `elem` [MyMove, OponentsMove]) $ do
                 menuItem ctxMenu [ text := "Offer draw", on command := hPutStrLn h "4 draw" ]
                 menuItem ctxMenu [ text := "Resign", on command := hPutStrLn h "4 resign" ]
                 return ()

  set p_back [ on clickRight := (\pt -> menuPopup ctxMenu pt p_back)]
  set p_board [ on clickRight := (\pt -> menuPopup ctxMenu pt p_board) ]

  -- layout
  set p_back [ layout := layoutBoardF color ]
  set f [ statusBar := [status]]
  refit p_back

  threadId <- forkIO $ eventLoop eventId chan vCmd f
  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd -> case cmd of

    GameMove move' -> when (gameId move' == gameId move) $ do
                                   state <- varGet vBoardState
                                   varSet vBoardState $ state { Board._position = position move'
                                                              , Board.isInteractive = relation move' == MyMove}
                                   repaint (Board._panel state)
                                   set t_white [enabled := True]
                                   set t_black [enabled := True]
                                   varSet vClock move'
                                   set status [text := ""]
                                   modifyMVar_ vMoves $ return . addMove move'

    GameResult id reason result -> when (id == gameId move) $ do
                              state <- varGet vBoardState
                              varSet vBoardState $ state {Board.isInteractive = False}
                              set t_white [enabled := False]
                              set t_black [enabled := False]
                              set status [text := (show result ++ ": " ++ reason)]
                              swapMVar vGameResult $ Just result
                              hPutStrLn h "4 iset seekinfo 1"
                              killThread threadId

    DrawOffered -> when (isGameUser move) $
                     set status [text := nameOponent color move ++ " offered a draw. Accept? (y/n)"] >>
                     set p_back [on (charKey 'y') := hPutStrLn h "5 accept" >> unsetKeyHandler p_back] >>
                     set p_back [on (charKey 'n') := hPutStrLn h "5 decline" >> unsetKeyHandler p_back]

    DrawDeclined -> when (isGameUser move) $
                      set status [text := nameOponent color move ++ " declined your draw offer."]

    _ -> return ()

  windowOnDestroy f $ do catch (killThread threadId) (\e -> print $ show (e :: IOException))
                         gameResult <- readMVar vGameResult
                         moves <- readMVar vMoves
                         saveAsPGN (reverse moves) gameResult
                         unless (isJust gameResult) $ case relation move of
                           MyMove -> hPutStrLn h "5 resign"
                           OponentsMove -> hPutStrLn h "5 resign"
                           Observing -> hPutStrLn h $ "5 unobserve " ++ show (gameId move)
                           _ -> return ()


turnBoard :: Var Board.BoardState -> Panel () -> (PColor -> Layout) -> IO ()
turnBoard vState p layoutF = do
  state <- varGet vState
  let color' = invert $ Board.perspective state
  varSet vState $ state { Board.perspective = color' }
  set p [layout := layoutF color']
  repaint (Board._panel state)
  refit p


createStatusPanel :: Panel () -> Var Move -> PColor -> IO (Panel (), Graphics.UI.WX.Timer)
createStatusPanel p_back vMove color = do
  move <- varGet vMove
  p_status <- panel p_back []

  p_color <- panel p_status [bgcolor := toWxColor color]
  st_clock <- staticTextFormatted p_status (formatTime $ remainingTime color move)
  st_playerName <- staticTextFormatted p_status (namePlayer color move)

  t <- timer p_back [ interval := 1000
                    , on command := updateTime color vMove st_clock
                    , enabled := isJust $ movePretty move ]

  set p_status [ layout := row 10 [ valignCenter $ minsize (Size 18 18) $ widget p_color
                                  , widget st_clock
                                  , widget st_playerName] ]
  return (p_status, t)


updateTime :: PColor -> Var Move -> StaticText () -> IO ()
updateTime color vClock st = do
  move <- changeRemainingTime color `fmap` varGet vClock
  varSet vClock move
  set st [text := formatTime $ remainingTime color move]
  where
    changeRemainingTime color move = if color == turn move
      then decreaseRemainingTime color move else move


staticTextFormatted :: Panel () -> String -> IO (StaticText ())
staticTextFormatted p s = staticText p [ text := s
                                       , fontFace := "Avenir Next Medium"
                                       , fontSize := 20
                                       , fontWeight := WeightBold]


layoutBoard :: Panel() -> Panel() -> Panel() -> PColor -> Layout
layoutBoard board white black color = grid 0 0 [ [hfill $ widget (if color == White then black else white)]
                                               , [fill $ minsize (Size 320 320) $ widget board]
                                               , [hfill $ widget (if color == White then white else black)]]


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

