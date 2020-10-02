module Macbeth.Fics.Parsers.Stored where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Stored
import           Macbeth.Fics.Parsers.Api

parser :: Parser Message
parser = StoredGames <$> (commandHead 127 *> stored)

stored :: Parser [Stored]
stored = emptyStored <|> manyStored

emptyStored :: Parser [Stored]
emptyStored = "Schoon has no adjourned games." *> pure []

manyStored :: Parser [Stored]
manyStored = "\nStored games for " *> many1 letter_ascii *> ":\n    C Opponent       On Type          Str  M    ECO Date\n" *> sepBy single (char '\n') 

single :: Parser Stored 
single = do
  sId <- skipSpace *> decimal <* ":"
  sColor <- skipSpace *> (("B" *> pure Black) <|> ("W" *> pure White)) 
  sOponent <- skipSpace *> (unpack <$> takeTill isSpace)
  sOn <- skipSpace *> (("N" *> pure False) <|> ("Y" *> pure True))
  sType <- skipSpace *> (unpack <$> ("[ " *> takeTill ((==) ']')) <* "]")
  sStrength <- skipSpace *> (unpack <$> takeTill isSpace)
  sNext <- skipSpace *> (unpack <$> takeTill isSpace)
  sECO <- skipSpace *> (unpack <$> takeTill isSpace)
  sDate <- skipSpace *> (unpack <$> takeTill ((==) '\n'))
  return Stored{..}

