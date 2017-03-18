{-# LANGUAGE DeriveGeneric #-}

module Macbeth.Wx.Config.BoardConfig (
  BoardConfig(..),
  defaultBoardConfig
) where

import Data.Yaml
import GHC.Generics

data BoardConfig = BoardConfig {
    showCapturedPieces :: Bool
} deriving (Show, Generic)

defaultBoardConfig :: BoardConfig
defaultBoardConfig = BoardConfig False

instance ToJSON BoardConfig
instance FromJSON BoardConfig

