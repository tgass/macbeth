module Macbeth.Fics.Api.Challenge (
  Challenge (..),
  PendingOffer(..),
  GameParams(..),
  Origin(..),
  isFrom,
  isTo,
  isUpdate,
  showChallenge,
  showShortGameParams
) where

import Macbeth.Fics.Api.Rating
import Macbeth.Fics.Api.Player


data Challenge = Challenge GameParams deriving (Show, Eq)


showChallenge :: Challenge -> String
showChallenge (Challenge p) =
  nameW p ++ " (" ++ show (ratingW p) ++ ") vs. " ++ nameB p ++ " (" ++ show (ratingB p) ++ ") "
  ++ showShortGameParams p


data Origin = From | To deriving (Show, Eq)


data PendingOffer = PendingOffer {
    origin :: Origin
  , offerId :: Int
  , playerName :: UserHandle
  , offerType :: String
  , gameParams :: GameParams } deriving (Show, Eq)


data GameParams = GameParams {
    nameW :: String
  , ratingW :: Rating
  , nameB :: String
  , ratingB :: Rating
  , rated :: Bool
  , speed :: String
  , initialTime :: Int
  , incTime :: Int } deriving (Show, Eq)


showShortGameParams :: GameParams -> String
showShortGameParams p = rated'' ++ " " ++ speed p ++ " " ++ show (initialTime p) ++ " " ++ show (incTime p)
  where rated'' = if rated p then "rated" else "unrated"


isFrom :: PendingOffer -> Bool
isFrom = (== From) . origin


isTo :: PendingOffer -> Bool
isTo = (== To) . origin


isUpdate :: Challenge -> Challenge -> Bool
isUpdate (Challenge p) (Challenge p') = (nameW p == nameW p') && (nameB p == nameB p')

