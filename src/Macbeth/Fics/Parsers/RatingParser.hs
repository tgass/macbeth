module Macbeth.Fics.Parsers.RatingParser (
  rating
) where

import Macbeth.Fics.Api.Rating

import Control.Applicative
import Data.Attoparsec.ByteString.Char8

rating :: Parser Rating
rating = (Rating <$> decimal <*> pure None) <|> "++++" *> pure Guest <|> "----" *> pure Unrated
