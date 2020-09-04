module Macbeth.Fics.Parsers.Api (
  commandHead,
  gameId
) where

import qualified Macbeth.Fics.Api.Api as Api

import Data.Attoparsec.ByteString.Char8
import Data.Char
import qualified Data.ByteString.Char8 as BS

gameId :: Parser Api.GameId
gameId = Api.GameId <$> decimal

data CommandHead = CommandHead Int deriving (Show)

commandHead :: Int -> Parser CommandHead
commandHead code = do
  _ <- char $ chr 21
  id' <- decimal
  _ <- char $ chr 22
  _ <- string $ BS.pack $ show code
  _ <- char $ chr 22
  return $ CommandHead id'


