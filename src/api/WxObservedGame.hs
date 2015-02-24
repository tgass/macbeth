module WxObservedGame (
--module Main (
  createObservedGame,
--  ,main
) where

import Api
import Board
import Utils (formatTime)

import Graphics.UI.WX
import Graphics.UI.WXCore

import Control.Concurrent
import Control.Concurrent.Chan

import Control.Applicative (liftA)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Chan
import CommandMsg


ficsEventId = wxID_HIGHEST + 53



main = start gui

gui = do
  f <- frame []
  vMove <- variable [ value := dummyMove ]
  --og <- createObservedGame vMove
  set f []



createObservedGame :: Move -> Chan CommandMsg -> IO ()
createObservedGame move chan = do
  f <- frame []
  vCmd <- newEmptyMVar

  p_back <- panel f []
  board <- createBoard p_back (Api.position move)

  vClock <- variable [ value := move ]

  -- panels
  let p_board = _panel board
  (p_white, t_white) <- createStatusPanel p_back vClock White
  (p_black, t_black) <- createStatusPanel p_back vClock Black

  let layoutBoardF = layoutBoard p_board p_white p_black

  -- context menu
  ctxMenu <- menuPane []
  menuItem ctxMenu [ text := "Turn board", on command := turnBoard board p_back layoutBoardF ]

  -- register paint callbacks
  set p_back [ on clickRight := (\pt -> menuPopup ctxMenu pt p_back)]
  set p_board [ on clickRight := (\pt -> menuPopup ctxMenu pt p_board) ]

  -- layout
  set p_back [layout := layoutBoardF White]

  refit p_back

  evtHandlerOnMenuCommand f ficsEventId $ takeMVar vCmd >>= \cmd -> do
    case cmd of

      MoveMsg move' -> if Api.gameId move' == Api.gameId move
                         then do
                           setPosition board (Api.position move')
                           varSet vClock move'
                         else return ()

      GameResultMsg id _ -> if id == Api.gameId move
                              then do
                                set t_white [enabled := False]
                                set t_black [enabled := False]
                              else return ()

      _ -> return ()


  windowShow f

  forkIO $ loop chan vCmd f
  return ()



loop :: Chan CommandMsg -> MVar CommandMsg -> Frame () -> IO ()
loop chan vCmd f = do
  cmd <- readChan chan
  putMVar vCmd cmd
  ev <- commandEventCreate wxEVT_COMMAND_MENU_SELECTED ficsEventId
  evtHandlerAddPendingEvent f ev
  loop chan vCmd f



createStatusPanel :: Panel () -> Var Move  -> Api.Color -> IO (Panel (), Graphics.UI.WX.Timer)
createStatusPanel p_back vMove color = do
  p_status <- panel p_back []
  p_color <- panel p_status [ bgcolor := if color == White then white else black]
  st_playerName <- createPlayerName p_status =<< namePlayer color `liftA` varGet vMove
  st_clock <- createClock p_status =<< remainingTime color `liftA` varGet vMove
  t <- timer p_back [ interval := 1000, on command := updateTime color vMove st_clock]
  set p_status [ layout := row 10 [ valignCenter $ minsize (Size 18 18) $ widget p_color
                                  , widget st_clock
                                  , widget st_playerName] ]
  return (p_status, t)



turnBoard :: Board -> Panel () -> (Api.Color -> Layout) -> IO ()
turnBoard board p layoutF = do
  color <- invertColor board
  set p [layout := layoutF color]
  repaint $ _panel board
  refit p



updateTime :: Api.Color -> Var Move -> StaticText () -> IO ()
updateTime color vClock st = do
  move <- changeRemainingTime color `liftA` varGet vClock
  varSet vClock move
  set st [text := formatTime $ remainingTime color move]
  where
    changeRemainingTime color move = if color == turn move
      then Api.decreaseRemainingTime color move else move



createPlayerName :: Panel () -> String -> IO (StaticText ())
createPlayerName p name = staticText p [ text := name
                                       , fontFace := "Avenir Next Medium"
                                       , fontSize := 20
                                       , fontWeight := WeightBold]



createClock :: Panel () -> Int -> IO (StaticText ())
createClock p time = staticText p [ text := formatTime time
                                  , fontFace := "Avenir Next Medium"
                                  , fontSize := 20
                                  , fontWeight := WeightBold]



layoutBoard :: Panel() -> Panel() -> Panel() -> Api.Color -> Layout
layoutBoard board white black color = (grid 0 0 [ [hfill $ widget (if color == White then black else white)]
                                                , [fill $ minsize (Size 320 320) $ widget board]
                                                , [hfill $ widget (if color == White then white else black)]])

dummyMove = Move {
    Api.position = [ (Square A One, Piece Rook White)
                   , (Square A Two, Piece Pawn White)
                   , (Square B Two, Piece Pawn White)
                   , (Square C Two, Piece Pawn White)
                   , (Square E Eight, Piece King Black)
                   , (Square D Eight, Piece Queen Black)
                   ],
    turn = Black,
    doublePawnPush = Nothing,
    Api.gameId = 1,
    nameW = "foobar",
    nameB = "barbaz",
    relation = Observing,
    moveNumber = 1,
    moveVerbose = "foo",
    timeTaken = "1:16",
    remainingTimeW = 113,
    remainingTimeB = 112,
    movePretty = "a4"
  }
