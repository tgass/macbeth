module Macbeth.Fics.Parsers.Api where

import           Data.Attoparsec.ByteString.Char8
import           Data.Char
import qualified Data.ByteString.Char8 as BS
import           Macbeth.Fics.Api.Api

gameId :: Parser GameId
gameId = GameId <$> decimal

commandHead :: Int -> Parser CommandId
commandHead code = do
  cid <- char (chr 21) *> decimal <* char (chr 22) <* string (BS.pack $ show code) <* char (chr 22)
  return $ CommandId cid

