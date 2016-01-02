module Macbeth.Fics.Api.PendingOffer (
  PendingOffer(..)
) where

data PendingOffer = PendingOffer { offerId :: Int, description :: String } deriving (Show, Eq)
