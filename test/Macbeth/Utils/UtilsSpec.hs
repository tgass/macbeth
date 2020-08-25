module Macbeth.Utils.UtilsSpec (spec) where

import Macbeth.Utils.Utils

import Test.Hspec

spec :: Spec
spec =
  describe "Tests" $ do
    it "encrypt" $ decrypt (encrypt "bar") `shouldBe` "bar"
