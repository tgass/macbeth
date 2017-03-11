{-# LANGUAGE DeriveGeneric #-}

module Macbeth.Wx.Config.GameConfig (
  GameConfig(..)
) where

import Data.Yaml
import GHC.Generics

data GameConfig = GameConfig {
    showCapturedPieces :: Maybe Bool
} deriving (Show, Generic)

instance ToJSON GameConfig
instance FromJSON GameConfig

