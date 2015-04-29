module WxObservedGame (
--module Main (
  createObservedGame,
--  ,main
) where

import Api
import Move
import Board
import CommandMsg
import Utils (formatTime)
import WxUtils

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Applicative (liftA)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Chan
import Data.Maybe (isJust)
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO (Handle, hPutStrLn)


eventId = wxID_HIGHEST + 53


createObservedGame :: Handle -> Move -> Api.Color -> Chan CommandMsg -> IO ()
createObservedGame h move color chan = do
  vCmd <- newEmptyMVar

  f <- frame []
  p_back <- panel f []
  board <- createBoard h p_back (Move.position move) color (relation move == MyMove)
  vClock <- variable [ value := move ]

  -- panels
  let p_board = _panel board
  (p_white, t_white) <- createStatusPanel p_back vClock White
  (p_black, t_black) <- createStatusPanel p_back vClock Black

  -- layout helper
  let layoutBoardF = layoutBoard p_board p_white p_black

  -- context menu
  ctxMenu <- menuPane []
  menuItem ctxMenu [ text := "Turn board", on command := turnBoard board p_back layoutBoardF ]
  menuItem ctxMenu [ text := "Resign", on command := resign h t_white t_black ]
  --menuItem ctxMenu [ text := "Offer draw", on command := hPutStrLn h "4 draw" ]

  set p_back [ on clickRight := (\pt -> menuPopup ctxMenu pt p_back)]
  set p_board [ on clickRight := (\pt -> menuPopup ctxMenu pt p_board) ]


  -- layout
  set p_back [layout := layoutBoardF color]
  refit p_back


  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd -> do
    case cmd of
      CommandMsg.Move move' -> when (Move.gameId move' == Move.gameId move) $ do
                                   setPosition board (Move.position move') (relation move' == MyMove)
                                   set t_white [enabled := True]
                                   set t_black [enabled := True]
                                   varSet vClock move'

      ConfirmMove move' -> when (Move.gameId move' == Move.gameId move) $ do
                                 setPosition board (Move.position move') (relation move' == MyMove)
                                 set t_white [enabled := True]
                                 set t_black [enabled := True]
                                 varSet vClock move'

      GameResult id r -> when (id == Move.gameId move) $ do
                              putStrLn $ "GameResult" ++ show id ++ " " ++ show r
                              set t_white [enabled := False]
                              set t_black [enabled := False]

      _ -> return ()


  windowShow f
  threadId <- forkIO $ eventLoop eventId chan vCmd f
  windowOnDestroy f $ do killThread threadId
                         -- TODO: Resign if playing
                         hPutStrLn h $ "5 unobserve " ++ (show $ Move.gameId move)

resign :: Handle -> Graphics.UI.WX.Timer -> Graphics.UI.WX.Timer-> IO ()
resign h t_white t_black = do hPutStrLn h "4 resign"
                   --           set t_white [enabled := False]
                     --         set t_black [enabled := False]

turnBoard :: Board -> Panel () -> (Api.Color -> Layout) -> IO ()
turnBoard board p layoutF = do
  color <- invertColor board
  set p [layout := layoutF color]
  repaint $ _panel board
  refit p


createStatusPanel :: Panel () -> Var Move -> Api.Color -> IO (Panel (), Graphics.UI.WX.Timer)
createStatusPanel p_back vMove color = do
  move <- varGet vMove
  p_status <- panel p_back []

  p_color <- panel p_status [bgcolor := toWxColor color]
  st_clock <- staticTextFormatted p_status (formatTime $ remainingTime color move)
  st_playerName <- staticTextFormatted p_status (namePlayer color move)

  t <- timer p_back [ interval := 1000
                    , on command := updateTime color vMove st_clock
                    -- TODO: disable if game is over (checkmate / Remis)
                    , enabled := isJust $ movePretty move ]

  set p_status [ layout := row 10 [ valignCenter $ minsize (Size 18 18) $ widget p_color
                                  , widget st_clock
                                  , widget st_playerName] ]
  return (p_status, t)


updateTime :: Api.Color -> Var Move -> StaticText () -> IO ()
updateTime color vClock st = do
  move <- changeRemainingTime color `liftA` varGet vClock
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


layoutBoard :: Panel() -> Panel() -> Panel() -> Api.Color -> Layout
layoutBoard board white black color = (grid 0 0 [ [hfill $ widget (if color == White then black else white)]
                                                , [fill $ minsize (Size 320 320) $ widget board]
                                                , [hfill $ widget (if color == White then white else black)]])


