module WxSink (
  wxSink
) where

import Api
import CommandMsg
import Game
import Move
import WxChallenge (wxChallenge)
import WxObservedGame (createObservedGame)

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import System.IO


wxSink :: Chan CommandMsg -> Handle -> CommandMsg -> IO ()
wxSink chan h cmd = do
    printCmdMsg cmd --TODO: -> FicsConnection
    case cmd of

      Observe move -> dupChan chan >>= createObservedGame h move White >>
                      writeChan chan cmd

      MatchAccepted move -> dupChan chan >>= createObservedGame h move (colorOfPlayer move) >>
                            writeChan chan cmd

      MatchRequested c -> dupChan chan >>= wxChallenge h c >>
                          writeChan chan cmd

      GameMove move' -> do writeChan chan cmd
                           when (isPlayersNewGame move') $ dupChan chan >>= createObservedGame h move' (colorOfPlayer move')
                           --TODO: -> FicsConnection
                           when (relation move' == OponentsMove && isCheckmate move') $ writeChan chan $ toGameResult move'

      Boxed cmds -> sequence_ $ fmap (writeChan chan) cmds

      _ -> writeChan chan cmd

toGameResult :: Move -> CommandMsg
toGameResult move = GameResult (Move.gameId move) (namePlayer colorTurn move ++ " checkmated") (turnToGameResult colorTurn)
  where
    colorTurn = turn move
    turnToGameResult Black = WhiteWins
    turnToGameResult White = BlackWins


printCmdMsg :: CommandMsg -> IO ()
printCmdMsg Prompt = return ()
printCmdMsg (NewSeek _) = return ()
printCmdMsg (RemoveSeeks _) = return ()
printCmdMsg cmd = print cmd
