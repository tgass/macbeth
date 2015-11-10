{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.RatingParser (
  rating
) where

import Macbeth.Api.Rating

import Control.Applicative
import Data.Attoparsec.ByteString.Char8

rating :: Parser Rating
rating = Rating <$> decimal <|> "++++" *> pure Guest <|> "----" *> pure Unrated
