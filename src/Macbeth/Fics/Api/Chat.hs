{-# LANGUAGE LambdaCase #-}

module Macbeth.Fics.Api.Chat (
    ChatMsg(..)
  , Status'(..)
  , belongsTo
) where

import qualified Macbeth.Fics.Api.Player as P
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Player

data ChatMsg =
    Say UserHandle GameId String
  | Tell UserHandle String
  | UserMessage Username String
  | Told P.UserHandle (Maybe Status')
  | OpenChat Username (Maybe GameId)
  | CloseChat Username deriving (Show, Eq)

data Status' = Playing | Busy String deriving (Show, Eq)

belongsTo :: ChatMsg -> Username -> Bool
belongsTo = \case
  Say (UserHandle u _) _ _ -> (u ==)
  Tell (UserHandle u _) _ -> (u ==)
  UserMessage u _ -> (u ==)
  _ -> const False
