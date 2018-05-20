{-# LANGUAGE OverloadedStrings #-}

module BoardConfigSpec (spec) where

import Macbeth.Wx.Config.BoardConfig

import Test.Hspec
import Data.Aeson


spec :: Spec
spec = do
  describe "Board Config Encoding" $ do

    it "default board config" $ encode defaultBoardConfig
      `shouldBe` "{\"blackTileConfig\":\"#b49664\",\"showCapturedPieces\":false,\"whiteTileConfig\":\"#ffffff\"}"

    it "padding hex" $ encode defaultBoardConfig{ whiteTileConfig = Just $ TileColor 0 0 0 }
      `shouldBe` "{\"blackTileConfig\":\"#b49664\",\"showCapturedPieces\":false,\"whiteTileConfig\":\"#000000\"}"

    it "tile file" $ encode defaultBoardConfig{ whiteTileConfig = Just $ TileFile "wood_blk.bmp" }
      `shouldBe` "{\"blackTileConfig\":\"#b49664\",\"showCapturedPieces\":false,\"whiteTileConfig\":\"wood_blk.bmp\"}"

    it "no tile config" $ encode (BoardConfig True Nothing Nothing)
      `shouldBe` "{\"blackTileConfig\":null,\"showCapturedPieces\":true,\"whiteTileConfig\":null}"


  describe "Board Config Decoding" $ do

    it "default board config" $ decode "{\"blackTileConfig\":\"#b49664\",\"showCapturedPieces\":false,\"whiteTileConfig\":\"#ffffff\"}"
      `shouldBe` Just (BoardConfig False (Just $ TileColor 255 255 255) (Just $ TileColor 180 150 100))

    it "board config with tile filepath" $ decode "{\"blackTileConfig\":\"wood_blk.bmp\",\"showCapturedPieces\":false,\"whiteTileConfig\":\"#ffffff\"}"
      `shouldBe` Just (BoardConfig False (Just $ TileColor 255 255 255) (Just $ TileFile "wood_blk.bmp"))

    it "board config with no tile config" $ decode "{\"showCapturedPieces\":false}"
      `shouldBe` Just (BoardConfig False Nothing Nothing)

