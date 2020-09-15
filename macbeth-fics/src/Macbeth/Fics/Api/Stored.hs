module Macbeth.Fics.Api.Stored where

import Macbeth.Fics.Api.Api

data Stored = Stored {
    sId :: Int
  , sColor :: PColor
  , sOponent :: String
  , sOn :: Bool
  , sType :: String
  , sStrength :: String
  , sNext :: String
  , sECO :: String
  , sDate :: String
  } deriving (Show, Eq)

