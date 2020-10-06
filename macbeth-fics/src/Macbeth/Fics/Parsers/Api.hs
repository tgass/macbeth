module Macbeth.Fics.Parsers.Api (
  commandHead,
  gameId
) where

import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.Char
import qualified Data.ByteString.Char8 as BS
import           Macbeth.Fics.Api.Api

gameId :: Parser GameId
gameId = GameId <$> decimal

data CommandHead = CommandHead Int deriving (Show)

commandHead :: Int -> Parser CommandHead
commandHead code = do
  void $ char $ chr 21
  commandId <- decimal
  void $ char $ chr 22
  void $ string $ BS.pack $ show code
  void $ char $ chr 22
  return $ CommandHead commandId


