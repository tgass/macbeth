{-# LANGUAGE OverloadedStrings #-}

module FicsConnectionSpec (spec) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.FicsConnection
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Move

import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List as CL
import Test.Hspec
import qualified Data.ByteString.Char8 as BS


spec :: Spec
spec =
  describe "FicsConnection" $ do

    it "test readId" $ readId "\NAK4\SYN158\SYNThat seek is not available.\n\r\ETB" `shouldBe` 158

    it "test crop" $ crop "\NAK4\SYN158\SYNThat seek is not available.\n\ETB" `shouldBe` "That seek is not available.\n"

    it "test unblock" $ runFicsConduit "\NAK4\SYN158\SYNThat seek is not available.\n\r\ETB" `shouldBe` [SeekNotAvailable]

    it "test conduit with nested messages" $ runFicsConduit 
      "\NAK4\SYN158\SYN\n\r<sr> 97\n\r\n\r<sr> 88\n\r\n\rCreating: chesspickle (1963) Schoon (1007) rated blitz 5 0\n\r{Game 70 (chesspickle vs. Schoon) Creating rated blitz match.}\n\r\n\r<12> -------- -------- -------- -------- -------- -------- -------- -------- W -1 1 1 1 1 0 70 chesspickle Schoon -1 5 0 39 39 300 300 1 none (0:00) none 1 0 0\n\r\n\rGame 70: A disconnection will be considered a forfeit.\n\r\ETB" 
     `shouldBe` [
        RemoveSeeks [97],
        RemoveSeeks [88],
        TextMessage "Creating: chesspickle (1963) Schoon (1007) rated blitz 5 0",
        GameCreation (GameProperties {gameId' = GameId 70, 
                                      playerW' = "chesspickle", 
                                      playerB' = "Schoon", 
                                      isGameUser' = True}),
        GameMove {Macbeth.Fics.FicsMessage.context = None, 
                  move = Move {positionRaw = "-------- -------- -------- -------- -------- -------- -------- --------", 
                  position = [],
                  turn = White, 
                  doublePawnPush = Nothing, 
                  castlingAv = [WhiteShort,WhiteLong,BlackShort,BlackLong], ply = 0, 
                  Macbeth.Fics.Api.Move.gameId = GameId 70, Macbeth.Fics.Api.Move.nameW = "chesspickle", 
                  Macbeth.Fics.Api.Move.nameB = "Schoon", relation = OponentsMove, initialTime = 5, incPerMove = 0, 
                  whiteRelStrength = 39, blackRelStrength = 39, remainingTimeW = 300, remainingTimeB = 300, 
                  moveNumber = 1, moveVerbose = Nothing, 
                  timeTaken = "(0:00)", 
                  movePretty = Nothing}},
        TextMessage "Game 70: A disconnection will be considered a forfeit."]

runFicsConduit :: BS.ByteString -> [FicsMessage]
runFicsConduit input = join $ (yield input) $$ linesC =$ blockC BS.empty =$ unblockC =$ parseC =$ CL.consume 

