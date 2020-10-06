module Macbeth.Fics.Parsers.Chatting (
  parser
) where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Parsers.Api
import qualified Macbeth.Fics.Parsers.Players as P
import           Macbeth.Fics.Parsers.RatingParser


parser :: Parser Message
parser = choice [
    says
  , tell
  , channelTell
  , kibitzes
  , whispers
  , told 
  , illegalWhisper
  , illegalSay
  ]


says :: Parser Message
says = do
  user <- P.userHandle
  mGameId <- (Just <$> ("[" *> gameId <* "]")) <|> pure Nothing
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
  gid <- "[" *> gameId <* "]"
  msg <- " kibitzes: " *> manyTill anyChar "\n"
  return $ Kibitzes user r gid msg


whispers :: Parser Message
whispers = do
  user <- P.userHandle <* "("
  r <- rating <* ")"
  gid <- "[" *> gameId <* "]"
  msg <- " whispers: " *> manyTill anyChar "\n"
  return $ Whispers user r gid msg



told :: Parser Message
told = Told
  <$> ((commandHead 107 <|> commandHead 132) *> "(told " *> P.userHandle)
  <*> ((", " *> (Just <$> status)) <|> pure Nothing)


status :: Parser ChatStatus
status =
  "who is playing" *> pure Playing <|>
  (Busy <$> manyTill anyChar " (")


illegalWhisper :: Parser Message
illegalWhisper = 
      "You are not playing or observing a game." *> pure (IllegalWhisper Nothing)
  <|> ("You are not observing game " *> (IllegalWhisper . Just <$> gameId))


illegalSay :: Parser Message
illegalSay = "I don't know who to say that to." *> pure IllegalSay


