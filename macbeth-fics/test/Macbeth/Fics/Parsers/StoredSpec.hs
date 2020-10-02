module Macbeth.Fics.Parsers.StoredSpec (spec) where

import           Data.Attoparsec.ByteString (parseOnly)
import           Data.ByteString.Char8 hiding (length)
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Stored
import qualified Macbeth.Fics.Parsers.Stored as Stored
import           Test.Hspec

spec :: Spec
spec =
  describe "Parsing stored games" $ do
    it "Parsing a single stored game" $ parseOnly Stored.single stored `shouldBe` Right (Stored 1 White "TheDane" False "br  2  12" "0-0" "B2" "???" "Sun Nov 23,  6:14 CST 1997")
    it "Parsing a empty list of stored games" $ parseOnly Stored.stored flixxG `shouldBe` Right [Stored {sId = 1, sColor = White, sOponent = "Schoon", sOn = True, sType = "su 15   0", sStrength = "39-39", sNext = "B5", sECO = "D00", sDate = "Fri Oct  2, 09:49 EDT 2020"}]
    it "Parsing a list of stored games" $ parseOnly Stored.parser storedList `shouldBe` parsedList
    it "Parsing a empty list of stored games" $ parseOnly Stored.parser emptyList `shouldBe` Right (StoredGames [])


stored :: ByteString
stored = "  1: W TheDane       N [ br  2  12]  0-0  B2   ??? Sun Nov 23,  6:14 CST 1997\n"


storedList :: ByteString
storedList = "\NAK6\SYN127\SYN\nStored games for Schoon:\n    C Opponent       On Type          Str  M    ECO Date\n 1: W FlixxG          Y [ bu  5   0] 39-39 W2   C20 Thu Sep 17, 10:35 EDT 2020\n  1: W TheDane       N [ br  2  12]  0-0  B2   ??? Sun Nov 23,  6:14 CST 1997\n\ETB\n"

parsedList :: Either String Message
parsedList = Right $ StoredGames 
  [
    Stored {sId = 1, sColor = White, sOponent = "FlixxG", sOn = True, sType = "bu  5   0", sStrength = "39-39", sNext = "W2", sECO = "C20", sDate = "Thu Sep 17, 10:35 EDT 2020"}
  , Stored {sId = 1, sColor = White, sOponent = "TheDane", sOn = False, sType = "br  2  12", sStrength = "0-0", sNext = "B2", sECO = "???", sDate = "Sun Nov 23,  6:14 CST 1997"}
  ]

emptyList :: ByteString
emptyList = "\NAK6\SYN127\SYNSchoon has no adjourned games.\n\ETB\n"



flixxG :: ByteString
flixxG = "\nStored games for FlixxG:\n    C Opponent       On Type          Str  M    ECO Date\n 1: W Schoon          Y [ su 15   0] 39-39 B5   D00 Fri Oct  2, 09:49 EDT 2020\n"

