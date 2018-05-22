{-# LANGUAGE OverloadedStrings #-}

module BoardConfigSpec (spec) where

import Macbeth.Wx.Config.BoardConfig

import Test.Hspec
import Data.Aeson


spec :: Spec
spec = do
  describe "Board Config Encoding" $ do

    it "default board config" $ encode defaultBoardConfig
      `shouldBe` "{\"blackTile\":\"hexb49664\",\"showCapturedPieces\":false,\"whiteTile\":\"hexffffff\"}"

    it "padding hex" $ encode defaultBoardConfig{ whiteTile = Just $ TileRGB 0 0 0 }
      `shouldBe` "{\"blackTile\":\"hexb49664\",\"showCapturedPieces\":false,\"whiteTile\":\"hex000000\"}"

    it "tile file" $ encode defaultBoardConfig{ whiteTile = Just $ TileFile "wood_blk.bmp" }
      `shouldBe` "{\"blackTile\":\"hexb49664\",\"showCapturedPieces\":false,\"whiteTile\":\"wood_blk.bmp\"}"

    it "no tile config" $ encode (BoardConfig True Nothing Nothing :: BoardConfigFormat)
      `shouldBe` "{\"blackTile\":null,\"showCapturedPieces\":true,\"whiteTile\":null}"


  describe "Board Config Decoding" $ do

    it "default board config" $ decode "{\"blackTile\":\"hexb49664\",\"showCapturedPieces\":false,\"whiteTile\":\"hexffffff\"}"
      `shouldBe` Just (BoardConfig False (Just $ TileRGB 255 255 255) (Just $ TileRGB 180 150 100))

    it "board config with tile filepath" $ decode "{\"blackTile\":\"wood_blk.bmp\",\"showCapturedPieces\":false,\"whiteTile\":\"hexffffff\"}"
      `shouldBe` Just (BoardConfig False (Just $ TileRGB 255 255 255) (Just $ TileFile "wood_blk.bmp"))

    it "invalid color hex value" $ decode "{\"blackTile\":\"wood_blk.bmp\",\"showCapturedPieces\":false,\"whiteTile\":\"hexXXXXXX\"}"
      `shouldBe` (Nothing :: Maybe BoardConfigFormat)

    it "board config with no tile config" $ decode "{\"showCapturedPieces\":false}"
      `shouldBe` Just (BoardConfig False Nothing Nothing :: BoardConfigFormat)

