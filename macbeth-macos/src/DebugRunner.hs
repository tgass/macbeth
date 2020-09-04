module Main where

import           Control.Concurrent.Chan
import           Graphics.UI.WX
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Player
import qualified Macbeth.Fics.Api.Game as G
import qualified Macbeth.Fics.Api.Move as M
import           Macbeth.Wx.RuntimeEnv
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Rating
import           Macbeth.Wx.Game.Game
import           Sound.ALUT hiding (Square)
import           System.IO

main :: IO ()
main = withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
  env <- initRuntime stdout
  setUsername env $ UserHandle "foo" []
  chan <- newChan
  writeChan chan message
  start $ wxGame env (GameId 1) defaultGameParams chan
  
defaultGameParams :: G.GameParams
defaultGameParams = G.GameParams True "foo" Unrated "bar" Unrated True "hmm" 60 1

message :: Message
message = GameMove M.None $ initMove' (GameId 1) defaultGameParams

initMove' :: GameId -> G.GameParams -> M.Move
initMove' gameId' gameParams' = M.Move {
    M.positionRaw = ""
  , M.position = [(Square B One, Piece King White), (Square B Two, Piece King Black)]
  , M.turn = White
  , M.doublePawnPush = Nothing
  , M.castlingAv = []
  , M.ply = 0
  , M.gameId = gameId'
  , M.nameW = G.nameW gameParams'
  , M.nameB = G.nameB gameParams'
  , M.relation = M.MyMove
  , M.moveNumber = 0
  , M.moveVerbose = Just $ Simple (Square A Two) (Square A One)
  , M.timeTaken = "0"
  , M.remainingTimeW = 0
  , M.remainingTimeB = 0
  , M.movePretty = Just "someCheck+"
  , M.initialTime = 0
  , M.incPerMove = 0
  , M.whiteRelStrength = 0
  , M.blackRelStrength = 0 }

