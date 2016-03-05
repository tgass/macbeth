module Macbeth.Fics.Api.PendingOffer (
  PendingOffer(..),
  Origin (..),
  isFrom,
  isTo
) where

import Macbeth.Fics.Api.Player

data Origin = From | To deriving (Show, Eq)

data PendingOffer = PendingOffer {
    origin :: Origin
  , offerId :: Int
  , playerName :: UserHandle
  , offerType :: String
  , params :: String } deriving (Show, Eq)

isFrom :: PendingOffer -> Bool
isFrom = (== From) . origin

isTo :: PendingOffer -> Bool
isTo = (== To) . origin
