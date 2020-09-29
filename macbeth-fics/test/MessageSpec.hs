module MessageSpec (spec) where

import Macbeth.Fics.Message
import Macbeth.Fics.Parsers.MessageParser
import Test.Hspec


spec :: Spec
spec =
  describe "MessageParser" $ do

    it "Parse log out message" $ parseMessage "Logging you out." `shouldBe` Right LogOut

    it "Parse abusive behavior" $ parseMessage "Due to abusive behavior, no registered users may use this server from your site.\n" `shouldBe` Right AbusiveBehavior

    it "Parse unobsere game" $ parseMessage "Removing game 8 from observation list." `shouldBe` Right (Unobserving $ GameId 8)


