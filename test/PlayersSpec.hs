module PlayersSpec (spec) where

import qualified Macbeth.Fics.Api.Rating as R
import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Player
import Macbeth.Fics.Parsers.Players

import Test.Hspec
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS


spec :: Spec
spec =
  describe "Players" $ do

    it "single player" $ parseOnly player' (BS.pack "++++^TeachMATE(U)\n")
      `shouldBe` Right (Player R.Guest InvolvedInAGame (Handle "TeachMATE" Unregistered))

    it "single player 2" $ parseOnly player' (BS.pack "1173.kalithkar  ")
      `shouldBe` Right (Player (R.Rating 1173 R.None) InactiveOrBusy (Handle "kalithkar" None))

    it "players" $ parseOnly players' (BS.pack "\n2985.BigMomma(C)                 ++++ xcx(U)\n1123^littledul                \n\n 1055 players displayed (of 1055). (*) indicates system administrator.\n\ETB")
      `shouldBe` Right [Player (R.Rating 2985 R.None) InactiveOrBusy (Handle "BigMomma" Computer),Player R.Guest NotBusy (Handle "xcx" Unregistered),Player (R.Rating 1123 R.None) InvolvedInAGame (Handle "littledul" None)]

    it "check num players in block" $ fmap (\(Players x) -> length x) (parseOnly players (BS.pack "\NAK5\SYN146\SYN\n2985.BigMomma(C)                 ++++ xcx(U)\n1123^littledul                \n\n 1055 players displayed (of 1055). (*) indicates system administrator.\n\ETB"))
      `shouldBe` Right 3

    it "check num players in block 2" $ fmap (\(Players x) -> length x) (parseOnly players (BS.pack "\NAK5\SYN146\SYN\n2985.BigMomma(C)                 ++++ xcx(U)\n\n 1055 players displayed (of 1055). (*) indicates system administrator.\n\ETB"))
      `shouldBe` Right 2

    it "partner not open for bughouse" $ parseOnly partnerNotOpen (BS.pack "\NAK6\SYN84\SYNzerowin is not open for bughouse.\n\ETB\n")
      `shouldBe` Right (PartnerNotOpen "zerowin")

