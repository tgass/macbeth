module Macbeth.Fics.Api.Game (
  Challenge (..),
  GameParams(..),
  toTitle,
  nameOponent,
  userColor,
  isUpdate,
  showChallenge,
  showShortGameParams,
  isGameWithPH
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Player
import Macbeth.Fics.Api.Rating


data Challenge = Challenge GameParams deriving (Show, Eq)


data GameParams = GameParams {
    isGameUser' :: Bool
  , nameW :: String
  , ratingW :: Rating
  , nameB :: String
  , ratingB :: Rating
  , rated :: Bool
  , gameType'' :: String
  , initialTime :: Int
  , incTime :: Int } deriving (Show, Eq)


nameOponent :: Username -> GameParams -> Maybe Username
nameOponent username' gameParams'
  | username' == nameW gameParams' = Just $ nameB gameParams'
  | username' == nameB gameParams' = Just $ nameW gameParams'
  | otherwise = Nothing


showShortGameParams :: GameParams -> String
showShortGameParams p = rated'' ++ " " ++ gameType'' p ++ " " ++ show (initialTime p) ++ " " ++ show (incTime p)
  where rated'' = if rated p then "rated" else "unrated"


showChallenge :: Challenge -> String
showChallenge (Challenge p) =
  nameW p ++ " (" ++ show (ratingW p) ++ ") vs. " ++ nameB p ++ " (" ++ show (ratingB p) ++ ") "
  ++ showShortGameParams p


isUpdate :: Challenge -> Challenge -> Bool
isUpdate (Challenge p) (Challenge p') = (nameW p == nameW p') && (nameB p == nameB p')


toTitle :: GameId -> GameParams -> String
toTitle (GameId gameId') params' =  "[Game " ++ show gameId' ++ "] " ++ nameW params' ++ " vs. " ++ nameB params'


userColor :: GameParams -> Username -> Maybe PColor
userColor gameParams' username'
  | nameW gameParams' == username' = Just White
  | nameB gameParams' == username' = Just Black
  | otherwise = Nothing


isGameWithPH :: GameParams -> Bool
isGameWithPH gp = gameType'' gp `elem` ["bughouse", "crazyhouse"]
