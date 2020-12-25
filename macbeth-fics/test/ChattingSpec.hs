module ChattingSpec where

import Macbeth.Fics.Message
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Player
import Macbeth.Fics.Api.Rating
import Macbeth.Fics.Parsers.MessageParser
import Test.Hspec

spec :: Spec
spec =
  describe "Chatting" $ do

    it "parse says" $ parseMessage "GuestYYPT(U)[386] says: hello\n"
      `shouldBe` Right (Says (UserHandle "GuestYYPT" [Unregistered]) (Just $ GameId 386) "hello")

    it "parse tell" $ parseMessage "GuestQRDR(U) tells you: foobar\n"
      `shouldBe` Right (Tells (UserHandle "GuestQRDR" [Unregistered]) Nothing "foobar")

    it "parse channel tell" $ parseMessage "simonv(3): Unfortunately Schoon, there is not yet any Klingon channel on FICS. :o)\n"
      `shouldBe` Right (Tells (UserHandle "simonv" []) (Just $ ChannelId 3) "Unfortunately Schoon, there is not yet any Klingon channel on FICS. :o)")

    it "parse kibitzes" $ parseMessage "FlixxG(----)[5] kibitzes: Hi Matt- great game!\n"
      `shouldBe` Right (Kibitzes (UserHandle "FlixxG" []) Unrated (GameId 5) "Hi Matt- great game!")

    it "parse whispers" $ parseMessage "FlixxG(----)[5] whispers: good move!\n"
      `shouldBe` Right (Whispers (UserHandle "FlixxG" []) Unrated (GameId 5) "good move!")

    it "parse told" $ parseMessage "\NAK5\SYN107\SYN(told GuestQRDR, who is playing)\n\ETB\n"
      `shouldBe` Right (Told (CommandId 5) (UserHandle "GuestQRDR" []) (Just Playing))

    it "parse told" $ parseMessage "\NAK7\SYN132\SYN(told GuestVHJJ)\n\ETB\n"
      `shouldBe` Right (Told (CommandId 7) (UserHandle "GuestVHJJ" []) Nothing)

    it "parse told" $ parseMessage "\NAK5\SYN132\SYN(told GuestYZFM, who real busy (idle: 19 secs))\n\ETB\n"
      `shouldBe` Right (Told (CommandId 5) (UserHandle "GuestYZFM" []) (Just $ Busy "who real busy"))
