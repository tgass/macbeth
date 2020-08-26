{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Macbeth.Wx.Config.SeekConfig where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson.Types
import           Data.Maybe
import           Macbeth.Utils.Utils
import           Macbeth.Fics.Api.Seek
import           Macbeth.Fics.Api.GameType 

type SeekConfigFormat = SeekConfig' (Maybe Category) (Maybe WildBoard) (Maybe SeekColor) (Maybe Int) (Maybe Bool)

convert :: SeekConfigFormat -> SeekConfig
convert scf = SeekConfig 
  (fromMaybe defaultCategory $ scf ^. scCategory)
  (scf ^. scBoard) 
  (fromMaybe defaultColor $ scf ^. scColor)
  (fromMaybe defaultTime $ scf ^. scTime)
  (fromMaybe defaultInc $ scf ^. scInc)
  (fromMaybe defaultRated $ scf ^. scRated)
  (fromMaybe defaultManual $ scf ^. scManual)
  (fromMaybe defaultRatingFrom $ scf ^. scRatingFrom)
  (fromMaybe defaultRatingTo $ scf ^. scRatingTo)

convertToFormat :: SeekConfig -> SeekConfigFormat
convertToFormat sc = SeekConfig
  (Just $ sc ^. scCategory) 
  (sc ^. scBoard)
  (Just $ sc ^. scColor)
  (Just $ sc ^. scTime) 
  (Just $ sc ^. scInc) 
  (Just $ sc ^. scRated) 
  (Just $ sc ^. scManual) 
  (Just $ sc ^. scRatingFrom) 
  (Just $ sc ^. scRatingTo)

setDefault :: SeekConfigFormat -> SeekConfigFormat
setDefault s = s & scCategory .~ (s ^. scCategory <|> Just defaultCategory)
                 & scBoard .~ (s ^. scBoard <|> defaultWildBoard)
                 & scColor .~ (s ^. scColor <|> Just defaultColor)
                 & scTime .~ (s ^. scTime <|> Just defaultTime)
                 & scInc .~ (s ^. scInc <|> Just defaultInc)
                 & scRated .~ (s ^. scRated <|> Just defaultRated)
                 & scManual .~ (s ^. scManual <|> Just defaultManual)
                 & scRatingFrom .~ (s ^. scRatingFrom <|> Just defaultRatingFrom)
                 & scRatingTo .~ (s ^. scRatingTo <|> Just defaultRatingTo)
  

defaultCategory :: Category
defaultCategory = Chess

defaultWildBoard :: Maybe (WildBoard)
defaultWildBoard = Nothing

defaultColor :: SeekColor
defaultColor = Automatic

defaultTime :: Int
defaultTime = 5

defaultInc :: Int
defaultInc = 0

defaultRated :: Bool
defaultRated = False

defaultManual :: Bool
defaultManual = False

defaultRatingFrom :: Int
defaultRatingFrom = 0

defaultRatingTo :: Int
defaultRatingTo = 9999

defaultFormat :: SeekConfigFormat
defaultFormat = SeekConfig
  (Just defaultCategory) 
  defaultWildBoard 
  (Just defaultColor)
  (Just defaultTime) 
  (Just defaultInc) 
  (Just defaultRated) 
  (Just defaultManual) 
  (Just defaultRatingFrom) 
  (Just defaultRatingTo)


instance ToJSON SeekConfigFormat where
  toJSON = genericToJSON (customOptions "_sc")

instance FromJSON SeekConfigFormat where
  parseJSON = genericParseJSON (customOptions "_sc")

