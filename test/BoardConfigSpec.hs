module BoardConfigSpec (spec) where

import Data.Aeson
import Macbeth.Wx.Config.BoardConfig
import Test.Hspec


spec :: Spec
spec = do
  describe "Board Config Encoding" $ do

    it "default board config" $ encode defaultBoardConfig
      `shouldBe` "{\"highlightConfig\":{\"moveColor\":\"hex0000ff\",\"style\":\"hatched\",\"preMoveColor\":\"hex00ff00\",\"checkColor\":\"hexff0000\"},\"pieceSet\":\"Alpha1\",\"blackTile\":\"hexb49664\",\"boardSize\":320,\"showCapturedPieces\":false,\"whiteTile\":\"hexffffff\"}"
    it "color tile" $ encode (TileRGB $ ColorRGB 0 0 0) `shouldBe` "\"hex000000\""

    it "img tile" $ encode (TileFile "wood_blk.bmp") `shouldBe` "\"wood_blk.bmp\""


  describe "Board Config Decoding" $ do

    it "default board config" $ decode "{\"blackTile\":\"hexb49664\",\"showCapturedPieces\":false,\"whiteTile\":\"hexffffff\"}"
      `shouldBe` Just (BoardConfig False (Just $ TileRGB $ ColorRGB 255 255 255) (Just $ TileRGB $ ColorRGB 180 150 100) Nothing Nothing Nothing:: BoardConfigFormat)

    it "img tile" $ decode "\"wood_blk.bmp\"" `shouldBe` (Just $ TileFile "wood_blk.bmp")

    it "invalid color tile" $ decode "\"hexXXXXXX\"" `shouldBe` (Nothing :: Maybe TileFormat)

  describe "ToJSON/FromJSON Highlight" $ do
    let hatched = HighlightConfig Hatched (ColorRGB 100 200 100) (ColorRGB 100 200 100) (ColorRGB 0 20 50)
    it "hatched identity" $ decode (encode hatched) `shouldBe` Just hatched
