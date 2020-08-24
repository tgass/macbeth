module Macbeth.Fics.Api.Seek where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.OngoingGame
import Macbeth.Fics.Api.Rating
import Macbeth.Fics.Utils.Bitmask

data Seek = Seek {
    id :: Int
  , name :: String
  , titles :: [Title] -- It is unlikely the titles will ever be mixed though as they should be exclusive. However the server does allow this.
  , rating :: Rating
  , timeStart :: Int
  , timeIncPerMove :: Int
  , isRated :: Bool
  , gameType :: GameType
  , color :: Maybe PColor
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
