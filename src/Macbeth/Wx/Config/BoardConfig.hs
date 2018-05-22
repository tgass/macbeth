{-# LANGUAGE OverloadedStrings, DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}

module Macbeth.Wx.Config.BoardConfig (
  BoardConfig'(..),
  BoardConfigFormat,
  BoardConfig, 
  Tile(..),
  TileFormat(..),
  defaultBoardConfig,
  convert
) where

import           Data.Aeson.Types
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Read (hexadecimal)
import           Macbeth.Wx.Utils (getUserOrAppFile)
import           Numeric
import           GHC.Generics
import           Graphics.UI.WX 
import           System.FilePath ((</>))

data BoardConfig' a = BoardConfig {
    showCapturedPieces :: Bool
  , whiteTile :: a
  , blackTile :: a
} deriving (Show, Generic, Eq)

type BoardConfig = BoardConfig' Tile

data Tile = BitmapTile (Bitmap ()) | ColorTile Color deriving (Show, Eq)

type BoardConfigFormat = BoardConfig' (Maybe TileFormat) 

data TileFormat = TileRGB Int Int Int | TileFile FilePath deriving (Show, Eq)

convert :: BoardConfigFormat -> FilePath -> IO BoardConfig
convert c userDir = BoardConfig 
  <$> pure (showCapturedPieces c) 
  <*> convertTile userDir (fromMaybe defaultWhiteTile $ whiteTile c) 
  <*> convertTile userDir (fromMaybe defaultBlackTile $ blackTile c)

convertTile :: FilePath -> TileFormat -> IO Tile
convertTile _ (TileRGB c1 c2 c3) = return $ ColorTile $ rgb c1 c2 c3
convertTile userDir (TileFile filename') = (BitmapTile . bitmap) <$> getUserOrAppFile userDir ("tiles" </> filename')

defaultBoardConfig :: BoardConfigFormat
defaultBoardConfig = BoardConfig False (Just defaultWhiteTile) (Just defaultBlackTile)

defaultWhiteTile :: TileFormat
defaultWhiteTile = TileRGB 255 255 255

defaultBlackTile :: TileFormat
defaultBlackTile = TileRGB 180 150 100

instance ToJSON BoardConfigFormat
instance FromJSON BoardConfigFormat

instance FromJSON TileFormat where
 parseJSON val'@(String text') 
   | "hex" `T.isPrefixOf` text' = 
       if T.length text' == 9 
           then either (\_ -> typeMismatch "TileRGB" val') return $ parseTileRGB (T.drop 3 text')
           else typeMismatch "TileRGB" val'
-- TODO:  Check that file type is allowed: .ico, .bmp, .xpm, .png, .gif
--        isSuffixOf :: Eq a => [a] -> [a] -> Bool
   | otherwise = return $ TileFile $ T.unpack text'
 parseJSON v = typeMismatch "TileFormat" v

instance ToJSON TileFormat where
  toJSON (TileRGB c1 c2 c3) = String $ T.pack $ "hex" ++ (paddedHex c1 . paddedHex c2 . paddedHex c3) ""
  toJSON (TileFile path) = String $ T.pack path

parseTileRGB :: T.Text -> Either String TileFormat
parseTileRGB t = TileRGB
  <$> (fst <$> hexadecimal (T.take 2 t))
  <*> (fst <$> hexadecimal (T.take 2 $ T.drop 2 t))
  <*> (fst <$> hexadecimal (T.take 2 $ T.drop 4 t))

paddedHex :: Int -> ShowS
paddedHex x
  | x < 16 = showChar '0' . showHex x
  | otherwise = showHex x
