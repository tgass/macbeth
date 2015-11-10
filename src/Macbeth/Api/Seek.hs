module Macbeth.Api.Seek (
  Seek (..),
  StartMode
) where

import Macbeth.Api.Api
import Macbeth.Api.Game
import Macbeth.Api.Rating

data Seek = Seek {
    id :: Int
  , name :: String
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
