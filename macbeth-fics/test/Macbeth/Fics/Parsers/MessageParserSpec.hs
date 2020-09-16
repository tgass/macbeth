module Macbeth.Fics.Parsers.MessageParserSpec (spec) where

import           Data.Attoparsec.ByteString.Char8
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Message
import           Macbeth.Fics.Parsers.MessageParser
import           Test.Hspec

spec :: Spec
spec =
  describe "MessageParsers" $ do

    it "Continue adjourned" $ parseMessage "{Game 124 (FlixxG vs. Schoon) Continuing rated blitz match.}\n" `shouldBe` Right (NewGameIdUser (GameId 124))


