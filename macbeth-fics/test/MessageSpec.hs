module MessageSpec (spec) where

import Macbeth.Fics.Message
import Macbeth.Fics.Parsers.MessageParser
import Test.Hspec


spec :: Spec
spec =
  describe "MessageParser" $ do
    it "Parse log out message" $ parseMessage "Logging you out." `shouldBe` Right LogOut
