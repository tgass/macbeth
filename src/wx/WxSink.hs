module WxSink (
  wxSink
) where

import Api
import CommandMsg
import Move
import WxChallenge (wxChallenge)
import WxObservedGame (createObservedGame)

import Control.Concurrent.Chan
import Control.Monad (when)
import System.IO (Handle)


wxSink :: Chan CommandMsg -> Handle -> CommandMsg -> IO ()
wxSink chan h cmd = case cmd of

  Observe move -> writeChan chan cmd >> dupChan chan >>= createObservedGame h move White

  MatchAccepted move -> writeChan chan cmd >> dupChan chan >>= createObservedGame h move (colorOfPlayer move)

  MatchRequested c -> writeChan chan cmd >> dupChan chan >>= wxChallenge h c

  GameMove move' -> (when (isPlayersNewGame move') $ dupChan chan >>= createObservedGame h move' (colorOfPlayer move')) >>
                    writeChan chan cmd

  _ -> writeChan chan cmd
