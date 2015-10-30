{-# LANGUAGE OverloadedStrings #-}

module Lentils.Fics.Parsers.RatingParser (
  rating
) where

import Lentils.Api.Rating

import Control.Applicative
import Data.Attoparsec.ByteString.Char8

rating :: Parser Rating
rating = Rating <$> decimal <|> "++++" *> pure Guest <|> "----" *> pure Unrated
