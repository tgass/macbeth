module Seek (
  Seek (..),
  StartMode
) where

import Api

data Seek = Seek { id :: Int
                 , rating :: Rating
                 , name :: String
                 , timeStart :: Int
                 , timeIncPerMove :: Int
                 , isRated :: Bool
                 , gameType :: GameType
                 , color :: Maybe Color
                 , ratingRange :: (Int, Int)
--                 , startMode :: StartMode
--                 , checkFormula :: Bool
} deriving (Show)

data StartMode = Auto | Manual deriving (Show)
