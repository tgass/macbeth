module Macbeth.Fics.Parsers.Api (
  commandHead
) where

import Data.Attoparsec.ByteString.Char8
import Data.Char
import qualified Data.ByteString.Char8 as BS

data CommandHead = CommandHead Int deriving (Show)

commandHead :: Int -> Parser CommandHead
commandHead code = do
  char $ chr 21
  id <- decimal
  char $ chr 22
  string $ BS.pack $ show code
  char $ chr 22
  return $ CommandHead id


