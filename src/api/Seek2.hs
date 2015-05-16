module Seek2 (
  Seek2 (..),
  StartMode
) where

import Api

data Seek2 = Seek2 { id :: Int
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

instance Show Seek2 where
  show (Seek2 id _ _ _ _ _ _ _ _) = "Seek2 {id = " ++ show id ++ "}"

data StartMode = Auto | Manual deriving (Show)
