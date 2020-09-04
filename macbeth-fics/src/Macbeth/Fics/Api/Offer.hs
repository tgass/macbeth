module Macbeth.Fics.Api.Offer where

import Macbeth.Fics.Api.Player

data PendingOffer = PendingOffer {
    origin :: Origin
  , offerId :: Int
  , playerName :: UserHandle
  , offerType :: String
  , offerDetails :: String } deriving (Show, Eq)


data Origin = From | To deriving (Show, Eq)


data OfferSubject = DrawReq | TakebackReq | AbortReq | MatchReq deriving Eq


instance Show OfferSubject where
  show DrawReq = "draw"
  show TakebackReq = "takeback"
  show AbortReq = "abort"
  show MatchReq = "match"


isFrom :: PendingOffer -> Bool
isFrom = (== From) . origin


isTo :: PendingOffer -> Bool
isTo = (== To) . origin
