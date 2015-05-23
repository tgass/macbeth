module Seek (
  Seek (..),
  StartMode
) where

import Api

data Seek = Seek { id :: Int
                 , name :: String
                 , rating :: Rating
                 , timeStart :: Int
                 , timeIncPerMove :: Int
                 , isRated :: Bool
                 , gameType :: GameType
                 , color :: Maybe Color
                 , ratingRange :: (Int, Int)
--                 , startMode :: StartMode
--                 , checkFormula :: Bool
}

instance Show Seek where
  show (Seek id _ _ _ _ _ _ _ _) = "Seek2 {id = " ++ show id ++ "}"

data StartMode = Auto | Manual deriving (Show)
