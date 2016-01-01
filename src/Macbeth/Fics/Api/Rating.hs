module Macbeth.Fics.Api.Rating (
  Rating (..),
  ProvShow (..)
) where

data Rating = Rating {r :: Int, provShow :: ProvShow} | Unrated | Guest deriving (Eq)

data ProvShow = None | Estimated | Provisional deriving (Eq)

instance Show ProvShow where
  show None = ""
  show Estimated = "E"
  show Provisional = "P"

instance Show Rating where
  show (Rating r' ps) = Prelude.show r' ++ Prelude.show ps
  show Guest = "Guest"
  show Unrated = "Unrated"

