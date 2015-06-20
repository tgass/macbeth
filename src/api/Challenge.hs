module Challenge (
  Challenge (..),
  displayChallenge
) where

import Rating

data Challenge = Challenge { nameW :: String
                           , ratingW :: Rating
                           , nameB :: String
                           , ratingB :: Rating
                           , params :: String } deriving (Show)

displayChallenge :: Challenge -> String
displayChallenge c = (nameW c) ++ " " ++ (show $ ratingW c) ++ " vs. " ++ (nameB c) ++ " (" ++ (show $ ratingB c) ++ ") " ++ (params c)
