module Macbeth.Wx.Config.BoardConfig where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Read (hexadecimal)
import           Macbeth.Fics.Api.Api
import           Macbeth.Wx.Game.PieceSet
import           Numeric
import           GHC.Generics
import           Graphics.UI.WX hiding (style)
import qualified Paths 

data BoardConfig' a b c d e = BoardConfig {
    showCapturedPieces :: Bool
  , whiteTile :: a
  , blackTile :: a
  , boardSize :: b
  , pieceSet :: c
  , highlightConfig :: d
  , showLabels :: e
} deriving (Show, Eq, Generic)

type BoardConfig = BoardConfig' Tile Int PieceSet HighlightConfig Bool

type BoardConfigFormat = BoardConfig' (Maybe TileFormat) (Maybe Int) (Maybe PieceSet) (Maybe HighlightConfig) (Maybe Bool)

data Tile = BitmapTile (Bitmap ()) | ColorTile Color deriving (Show, Eq)

data TileFormat = TileRGB ColorRGB | TileFile FilePath deriving (Show, Eq)

data HighlightConfig = HighlightConfig {
    style :: HighlightStyle
  , moveColor :: ColorRGB
  , preMoveColor :: ColorRGB
  , checkColor :: ColorRGB
  , mouseColor :: Maybe ColorRGB
  } deriving (Show, Eq, Generic)

data HighlightStyle = Hatched | Solid deriving (Show, Generic, Eq)

data ColorRGB = ColorRGB Int Int Int deriving (Show, Eq)

isSolidStyle :: HighlightConfig -> Bool
isSolidStyle conf = style conf == Solid

toSolidColor :: Square -> ColorRGB -> Color
toSolidColor square color 
  | squareColor square == White = convertColorRGB color
  | otherwise = convertColorRGB $ darkenColor color

toSolidColorReverse :: Square -> ColorRGB -> Color
toSolidColorReverse square color 
  | squareColor square == Black = convertColorRGB color
  | otherwise = convertColorRGB $ darkenColor color

convertColorRGB :: ColorRGB -> Color
convertColorRGB (ColorRGB r g b) = rgb r g b

darkenColor :: ColorRGB -> ColorRGB
darkenColor (ColorRGB r g b) = let (f :: Double) = 0.8 in ColorRGB (round $ f * fromIntegral r) (round $ f * fromIntegral g) (round $ f * fromIntegral b)

convert :: BoardConfigFormat -> FilePath -> IO BoardConfig
convert c userDir = BoardConfig 
  <$> pure (showCapturedPieces c) 
  <*> convertTile userDir (fromMaybe defaultWhiteTile $ whiteTile c) 
  <*> convertTile userDir (fromMaybe defaultBlackTile $ blackTile c)
  <*> pure (fromMaybe defaultBoardSize $ boardSize c)
  <*> pure (fromMaybe defaultPieceSet $ pieceSet c)
  <*> pure (fromMaybe defaultHighlightConfig $ highlightConfig c)
  <*> pure (fromMaybe defaultShowLabels $ showLabels c)

convertTile :: FilePath -> TileFormat -> IO Tile
convertTile _ (TileRGB (ColorRGB r g b)) = return $ ColorTile $ rgb r g b
convertTile userDir (TileFile filename) = (BitmapTile . bitmap) <$> Paths.getTileFilePath userDir filename

defaultBoardConfig :: BoardConfigFormat
defaultBoardConfig = BoardConfig False (Just defaultWhiteTile) (Just defaultBlackTile) (Just defaultBoardSize) (Just defaultPieceSet) (Just defaultHighlightConfig) (Just defaultShowLabels)

defaultWhiteTile :: TileFormat
defaultWhiteTile = TileRGB $ ColorRGB 255 255 255

defaultBlackTile :: TileFormat
defaultBlackTile = TileRGB $ ColorRGB 180 150 100

defaultBoardSize :: Int
defaultBoardSize = 320

defaultPieceSet :: PieceSet
defaultPieceSet = Alpha1

defaultHighlightConfig :: HighlightConfig
defaultHighlightConfig = HighlightConfig {
    style = Solid
  , moveColor = ColorRGB 204 206 132
  , preMoveColor = ColorRGB 128 125 133
  , checkColor = ColorRGB 229 108 97
  , mouseColor = Nothing
  }

defaultHighlightHatched :: HighlightConfig
defaultHighlightHatched = HighlightConfig {
    style = Hatched
  , moveColor = ColorRGB 0 0 255
  , preMoveColor = ColorRGB 0 255 0
  , checkColor = ColorRGB 255 0 0
  , mouseColor = Just $ ColorRGB 255 0 0
  }


defaultShowLabels :: Bool
defaultShowLabels = False

parseRGB :: Text -> Either String ColorRGB
parseRGB (T.stripPrefix "hex" -> Just t) = ColorRGB
  <$> (fst <$> hexadecimal (T.take 2 t))
  <*> (fst <$> hexadecimal (T.take 2 $ T.drop 2 t))
  <*> (fst <$> hexadecimal (T.take 2 $ T.drop 4 t))
parseRGB _ = Left "Wrong format. hex prefix missing."

toTextRGB :: ColorRGB -> Text
toTextRGB (ColorRGB r g b) = T.pack $ "hex" <> (paddedHex r . paddedHex g . paddedHex b) ""
  where
    paddedHex :: Int -> ShowS
    paddedHex x
      | x < 16 = showChar '0' . showHex x
      | otherwise = showHex x

instance ToJSON BoardConfigFormat
instance FromJSON BoardConfigFormat

instance ToJSON HighlightConfig
instance FromJSON HighlightConfig

instance ToJSON HighlightStyle where
  toJSON = genericToJSON $ defaultOptions { constructorTagModifier = fmap toLower }

instance FromJSON HighlightStyle where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = fmap toLower }

instance FromJSON TileFormat where
 parseJSON val@(String tile) 
   | ("hex" `T.isPrefixOf` tile) = either (const $ typeMismatch "TileRGB: wrong format" val) (return . TileRGB) $ parseRGB tile
-- TODO:  Check that file type is allowed: .ico, .bmp, .xpm, .png, .gif
--        isSuffixOf :: Eq a => [a] -> [a] -> Bool
   | otherwise = return $ TileFile $ T.unpack tile
 parseJSON val = typeMismatch "TileFormat" val

instance ToJSON TileFormat where
  toJSON (TileRGB colorRGB) = toJSON colorRGB
  toJSON (TileFile p) = String $ T.pack p

instance ToJSON ColorRGB where
  toJSON = String . toTextRGB

instance FromJSON ColorRGB where
 parseJSON val@(String colorString) 
   | ("hex" `T.isPrefixOf` colorString) && (T.length colorString == 9) 
       = either (const $ typeMismatch "ColorRGB: wrong format" val) return $ parseRGB colorString
   | otherwise = typeMismatch "ColorRGB: wrong format" val
 parseJSON val = typeMismatch "ColorRGB: wrong format" val

