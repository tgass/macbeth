module Macbeth.Fics.Parsers.RatingParser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Macbeth.Fics.Api.Rating

rating :: Parser Rating
rating = (Rating <$> decimal <*> pure None) <|> "++++" *> pure Guest <|> "----" *> pure Unrated
