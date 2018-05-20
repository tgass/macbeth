{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Macbeth.Wx.Config.BoardConfig (
  BoardConfig(..),
  defaultBoardConfig
) where

import           Data.Aeson.Types
import qualified Data.Text as T
import           Data.Text.Read (hexadecimal)
import           Numeric
import           GHC.Generics

data BoardConfig = BoardConfig {
    showCapturedPieces :: Bool
  , whiteTileConfig :: Maybe TileConfig
  , blackTileConfig :: Maybe TileConfig 
} deriving (Show, Generic)

defaultBoardConfig :: BoardConfig
defaultBoardConfig = BoardConfig False (Just $ TileColor 255 255 255) (Just $ TileColor 180 150 100)

instance ToJSON BoardConfig
instance FromJSON BoardConfig

data TileConfig = TileColor Int Int Int | TileFile FilePath deriving Show

instance FromJSON TileConfig where
 parseJSON val'@(String text') 
   | "#" `T.isPrefixOf` text' = 
       if T.length text' == 7 
           then either (\_ -> typeMismatch "TileColor" val') return $ parseTileColor text'
           else typeMismatch "TileColor" val'
   | otherwise = return $ TileFile $ T.unpack text'
 parseJSON v = typeMismatch "TileConfig" v

instance ToJSON TileConfig where
  toJSON (TileColor c1 c2 c3) = String $ T.pack $ (paddedHex c1 . paddedHex c2 . paddedHex c3) ""
  toJSON (TileFile path) = String $ T.pack path

parseTileColor :: T.Text -> Either String TileConfig
parseTileColor t = TileColor
  <$> (fst <$> hexadecimal (T.take 2 t))
  <*> (fst <$> hexadecimal (T.take 2 $ T.drop 2 t))
  <*> (fst <$> hexadecimal (T.take 2 $ T.drop 4 t))


paddedHex :: Int -> ShowS
paddedHex x
  | x < 16 = showChar '0' . showHex x
  | otherwise = showHex x
