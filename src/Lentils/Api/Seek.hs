module Lentils.Api.Seek (
  Seek (..),
  StartMode
) where

import Lentils.Api.Api
import Lentils.Api.Game
import Lentils.Api.Rating

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
}

instance Show Seek where
  show seek = "Seek {id = " ++ show (Lentils.Api.Seek.id seek) ++ "}"

data StartMode = Auto | Manual deriving (Show)
