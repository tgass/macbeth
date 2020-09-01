module SeekSpec (spec) where

import Macbeth.Fics.Api.GameType
import Macbeth.Fics.Api.Seek
import Macbeth.Fics.Commands.Seek

import Test.Hspec

seekInfo :: SeekConfig
seekInfo = SeekConfig {
    _scCategory = Chess
  , _scBoard = Nothing
  , _scTime = 5
  , _scInc = 10
  , _scRated = False
  , _scColor = Automatic
  , _scManual = False
  , _scRatingFrom = 0
  , _scRatingTo = 9999
}


spec :: Spec
spec =
  describe "Seek widget" $ do

    it "seekInfo to string" $ mkSeekString seekInfo `shouldBe` "seek 5 10 u chess a 0-9999"
    it "seekInfo to string, color=white" $ mkSeekString seekInfo {_scColor = White} `shouldBe` "seek 5 10 u w chess a 0-9999"
    it "seekInfo to string, color=white" $ mkSeekString seekInfo {_scRated = True} `shouldBe` "seek 5 10 r chess a 0-9999"
