module Macbeth.Wx.CommandHistorySpec (spec) where

import           Macbeth.Wx.CommandHistory (CommandHistory(..))
import qualified Macbeth.Wx.CommandHistory as History
import           Test.Hspec


history :: CommandHistory
history = History.push "fst" $ History.push "bar" History.empty


spec :: Spec
spec =
  describe "Testing Macbeth.Wx.CommandHistory..." $ do
    it "up with empty" $ fst (History.up History.empty) `shouldBe` Nothing

    it "down with empty" $ fst (History.down History.empty) `shouldBe` Nothing

    it "up" $ fst (History.up history) `shouldBe` Just "fst"

    it "up, up " $ fst (History.up $ snd $ History.up history) `shouldBe` Just "bar"

    it "up, up, up" $ fst (History.up $ snd $ History.up $ snd $ History.up history) `shouldBe` Just "bar"

    it "up, up, up, up " $ fst (History.up $ snd $ History.up $ snd $ History.up $ snd $ History.up history) `shouldBe` Just "bar"

    it "up, up, up, up, down " $ fst (History.down $ snd $ History.up $ snd $ History.up $ snd $ History.up $ snd $ History.up history) `shouldBe` Just "fst"

    it "up, up, up, up, down, down " $ fst (History.down $ snd $ History.down $ snd $ History.up $ snd $ History.up $ snd $ History.up $ snd $ History.up history) `shouldBe` Nothing

