{-# LANGUAGE OverloadedStrings #-}

module ParseUtils (
  rating
) where

import Api

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

rating :: Parser Rating
rating =
  (decimal >>= return . Rating) <|>
  string "++++" *> pure Guest <|>
  string "----" *> pure Unrated



