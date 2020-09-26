module Macbeth.Fics.Parsers.MessageParserSpec (spec) where

import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Offer
import           Macbeth.Fics.Message
import           Macbeth.Fics.Parsers.MessageParser
import           Test.Hspec

spec :: Spec
spec =
  describe "MessageParsers" $ do

    it "Continue adjourned" $ parseMessage "{Game 124 (FlixxG vs. Schoon) Continuing rated blitz match.}\n" `shouldBe` Right (NewGameId (GameId 124))
    it "Adjourn request declined" $ parseMessage "SGtanstafl declines the adjourn request.\n" `shouldBe` Right (OponentDecline "SGtanstafl" AdjournReq)



