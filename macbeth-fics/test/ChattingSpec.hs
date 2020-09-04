module ChattingSpec (spec) where

import Macbeth.Fics.Message
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Chat
import Macbeth.Fics.Api.Player
import Macbeth.Fics.Parsers.Chatting

import Test.Hspec
import Data.Attoparsec.ByteString.Char8

spec :: Spec
spec =
  describe "Chatting" $ do

    it "parse says" $ parseOnly says "GuestYYPT(U)[386] says: hello\n"
      `shouldBe` Right (Chat (Say (UserHandle "GuestYYPT" [Unregistered]) (GameId 386) "hello"))

    it "parse tell" $ parseOnly tell "GuestQRDR(U) tells you: foobar\n"
      `shouldBe` Right (Chat (Tell (UserHandle "GuestQRDR" [Unregistered]) "foobar"))

    it "parse told" $ parseOnly told "\NAK5\SYN107\SYN(told GuestQRDR, who is playing)\n\ETB\n"
      `shouldBe` Right (Chat (Told (UserHandle "GuestQRDR" []) (Just Playing)))

    it "parse told" $ parseOnly told "\NAK7\SYN132\SYN(told GuestVHJJ)\n\ETB\n"
      `shouldBe` Right (Chat (Told (UserHandle "GuestVHJJ" []) Nothing))

    it "parse told" $ parseOnly told "\NAK5\SYN132\SYN(told GuestYZFM, who real busy (idle: 19 secs))\n\ETB\n"
      `shouldBe` Right (Chat (Told (UserHandle "GuestYZFM" []) (Just $ Busy "who real busy")))
