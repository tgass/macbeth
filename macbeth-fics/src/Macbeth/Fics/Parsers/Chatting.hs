module Macbeth.Fics.Parsers.Chatting (
    says
  , tell
  , channelTell
  , kibitzes
  , whispers
  , told
) where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Api
import qualified Macbeth.Fics.Parsers.Api as Api
import qualified Macbeth.Fics.Parsers.Players as P
import           Macbeth.Fics.Parsers.RatingParser


says :: Parser Message
says = do
  user <- P.userHandle
  mGameId <- (Just <$> ("[" *> Api.gameId <* "]")) <|> pure Nothing
  msg <- " says: " *> manyTill anyChar "\n"
  return $ Says user mGameId msg


tell :: Parser Message
tell = do
  user <- P.userHandle
  msg <- " tells you: " *> manyTill anyChar "\n"
  return $ Tells user Nothing msg


channelTell :: Parser Message
channelTell = do 
  user <- P.userHandle <* "("
  cid <- ChannelId <$> (decimal <* "): ")
  msg <- manyTill anyChar "\n"
  return $ Tells user (Just cid) msg


kibitzes :: Parser Message
kibitzes = do
  user <- P.userHandle <* "("
  r <- rating <* ")"
  gameId <- "[" *> Api.gameId <* "]"
  msg <- " kibitzes: " *> manyTill anyChar "\n"
  return $ Kibitzes user r gameId msg


whispers :: Parser Message
whispers = do
  user <- P.userHandle <* "("
  r <- rating <* ")"
  gameId <- "[" *> Api.gameId <* "]"
  msg <- " whispers: " *> manyTill anyChar "\n"
  return $ Whispers user r gameId msg



told :: Parser Message
told = Told
  <$> ((Api.commandHead 107 <|> Api.commandHead 132) *> "(told " *> P.userHandle)
  <*> ((", " *> (Just <$> status)) <|> pure Nothing)


status :: Parser ChatStatus
status =
  "who is playing" *> pure Playing <|>
  (Busy <$> manyTill anyChar " (")
