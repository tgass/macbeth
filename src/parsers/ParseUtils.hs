{-# LANGUAGE OverloadedStrings #-}

module ParseUtils (
  rating
) where

import Api
import Rating

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

rating :: Parser Rating
rating =
  liftM Rating decimal <|>
  string "++++" *> pure Guest <|>
  string "----" *> pure Unrated



