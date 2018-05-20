{-# LANGUAGE OverloadedStrings #-}

module BoardConfigSpec (spec) where

import Macbeth.Wx.Config.BoardConfig

import Test.Hspec


spec :: Spec
spec =
  describe "Players" $

    it "encode default board config" $ encode defaultBoardConfig
      `shouldBe` ""

