module Macbeth.Fics.Parsers.Api (
  commandHead,
  gameId
) where

import qualified Macbeth.Fics.Api.Game as G

import Data.Attoparsec.ByteString.Char8
import Data.Char
import qualified Data.ByteString.Char8 as BS

gameId :: Parser G.GameId
gameId = G.GameId <$> decimal

data CommandHead = CommandHead Int deriving (Show)

commandHead :: Int -> Parser CommandHead
commandHead code = do
  _ <- char $ chr 21
  id' <- decimal
  _ <- char $ chr 22
  _ <- string $ BS.pack $ show code
  _ <- char $ chr 22
  return $ CommandHead id'


