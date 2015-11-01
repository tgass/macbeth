module Lentils.Api.Rating (
  Rating (..)
) where

data Rating = Rating {r :: Int} | Unrated | Guest deriving (Eq)

instance Show Rating where
  show (Rating r') = Prelude.show r'
  show Guest = "Guest"
  show Unrated = "Unrated"

