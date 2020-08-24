{-# LANGUAGE DeriveGeneric #-}

module Macbeth.Fics.Api.Seek where

import Data.Aeson
import Macbeth.Fics.Api.OngoingGame
import Macbeth.Fics.Api.Rating
import Macbeth.Fics.Utils.Bitmask
import GHC.Generics

data SeekColor = Automatic | White | Black deriving (Show, Read, Eq, Generic, Enum, Bounded)

data Seek = Seek {
    id :: Int
  , name :: String
  , titles :: [Title] -- It is unlikely the titles will ever be mixed though as they should be exclusive. However the server does allow this.
  , rating :: Rating
  , timeStart :: Int
  , timeIncPerMove :: Int
  , isRated :: Bool
  , gameType :: GameType
  , color :: SeekColor
  , ratingRange :: (Int, Int)
--                 , startMode :: StartMode
--                 , checkFormula :: Bool
} deriving (Eq, Show)

data StartMode = Auto | Manual deriving (Show)

data Title =   Unregistered
             | Computer
             | GrandMaster
             | InternationalMaster
             | FideMaster
             | WomenGrandMaster
             | WomenInternationalMaster
             | WomenFideMaster deriving (Bounded, Enum, Eq, Show)

instance ToBitMask Title

instance ToJSON SeekColor
instance FromJSON SeekColor

