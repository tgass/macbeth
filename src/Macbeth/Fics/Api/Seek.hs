{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Macbeth.Fics.Api.Seek where

import Control.Lens
import Data.Aeson
import Macbeth.Fics.Api.OngoingGame
import Macbeth.Fics.Api.Rating
import Macbeth.Fics.Api.GameType
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

data SeekConfig' a b c d e = SeekConfig {
    _scCategory :: a
  , _scBoard :: b
  , _scColor :: c
  , _scTime :: d
  , _scInc :: d
  , _scRated :: e
  , _scManual :: e
  , _scRatingFrom :: d
  , _scRatingTo :: d
} deriving (Show, Generic)

makeLenses ''SeekConfig'

type SeekConfig = SeekConfig' Category (Maybe WildBoard) SeekColor Int Bool

mkSeekString :: SeekConfig -> String
mkSeekString config = ("seek " ++) $ unwords $ filter (/= "") [
    show $ config ^. scTime
  , show $ config ^. scInc
  , convertIsRated $ config ^. scRated
  , convertColor $ config ^. scColor
  , gameTypeSelectionToString (config ^. scCategory) (config ^. scBoard)
  , convertIsManual $ config ^. scManual
  , convertRatingRange (config ^. scRatingFrom) (config ^. scRatingTo)
  ]

convertIsRated :: Bool -> String
convertIsRated True = "r"
convertIsRated False = "u"

convertColor :: SeekColor -> String
convertColor White = "w"
convertColor Black = "b"
convertColor Automatic = ""

convertRatingRange :: Int -> Int -> String
convertRatingRange from to = show from ++ "-" ++ show to

convertIsManual :: Bool -> String
convertIsManual True = "m"
convertIsManual False = "a"


instance ToBitMask Title

instance ToJSON SeekColor
instance FromJSON SeekColor

