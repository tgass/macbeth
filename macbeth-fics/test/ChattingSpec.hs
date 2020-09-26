module ChattingSpec where

import Macbeth.Fics.Message
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Chat
import Macbeth.Fics.Api.Player
import Macbeth.Fics.Parsers.MessageParser
import Test.Hspec

spec :: Spec
spec =
  describe "Chatting" $ do

    it "parse says" $ parseMessage "GuestYYPT(U)[386] says: hello\n"
      `shouldBe` Right (Says (UserHandle "GuestYYPT" [Unregistered]) (GameId 386) "hello")

    it "parse tell" $ parseMessage "GuestQRDR(U) tells you: foobar\n"
      `shouldBe` Right (Tells Nothing (UserHandle "GuestQRDR" [Unregistered]) "foobar")

    it "parse channel tell" $ parseMessage "simonv(3): Unfortunately Schoon, there is not yet any Klingon channel on FICS. :o)\n"
      `shouldBe` Right (Tells (Just $ ChannelId 3) (UserHandle "simonv" []) "Unfortunately Schoon, there is not yet any Klingon channel on FICS. :o)")

    it "parse told" $ parseMessage "\NAK5\SYN107\SYN(told GuestQRDR, who is playing)\n\ETB\n"
      `shouldBe` Right (Told (UserHandle "GuestQRDR" []) (Just Playing))

    it "parse told" $ parseMessage "\NAK7\SYN132\SYN(told GuestVHJJ)\n\ETB\n"
      `shouldBe` Right (Told (UserHandle "GuestVHJJ" []) Nothing)

    it "parse told" $ parseMessage "\NAK5\SYN132\SYN(told GuestYZFM, who real busy (idle: 19 secs))\n\ETB\n"
      `shouldBe` Right (Told (UserHandle "GuestYZFM" []) (Just $ Busy "who real busy"))
