{-# LANGUAGE OverloadedStrings #-}

module SeekSpec (spec) where

import Macbeth.Fics.Api.Api
import Macbeth.Wx.Seek
import Macbeth.Wx.GameType

import Test.Hspec
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

seekInfo :: SeekInfo
seekInfo = SeekInfo {
    _category = Chess
  , _board = Nothing
  , _time = 5
  , _inc = 10
  , _rated = False
  , _color = Nothing
  , _manual = False
  , _ratingFrom = 0
  , _ratingTo = 9999
}


spec :: Spec
spec =
  describe "Seek widget" $ do

    it "seekInfo to string" $ toString seekInfo `shouldBe` "5 10 u chess a 0-9999"
    it "seekInfo to string, color=white" $ toString seekInfo {_color = Just White} `shouldBe` "5 10 u w chess a 0-9999"
    it "seekInfo to string, color=white" $ toString seekInfo {_rated = True} `shouldBe` "5 10 r chess a 0-9999"

