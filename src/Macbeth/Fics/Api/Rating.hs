module Macbeth.Fics.Api.Rating (
  Rating (..),
  ProvShow (..)
) where

data Rating = Rating {r :: Int, provShow :: ProvShow} | Guest | Unrated deriving (Eq, Ord)

data ProvShow = None | Estimated | Provisional deriving (Eq, Ord)

instance Show ProvShow where
  show None = ""
  show Estimated = "E"
  show Provisional = "P"

instance Show Rating where
  show (Rating r' ps) = show r' ++ show ps
  show Guest = "Guest"
  show Unrated = "Unrated"

