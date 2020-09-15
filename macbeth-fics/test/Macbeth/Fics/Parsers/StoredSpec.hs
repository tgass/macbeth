module Macbeth.Fics.Parsers.StoredSpec (spec) where

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Stored
import qualified Macbeth.Fics.Parsers.Stored as Stored
import           Test.Hspec

spec :: Spec
spec =
  describe "Parsing stored games" $ do
    it "Parsing a single stored game" $ parseOnly Stored.single stored `shouldBe` Right (Stored 1 White "TheDane" False "br  2  12" "0-0" "B2" "???" "Sun Nov 23,  6:14 CST 1997")


stored :: ByteString
stored = "  1: W TheDane       N [ br  2  12]  0-0  B2   ??? Sun Nov 23,  6:14 CST 1997\n"
