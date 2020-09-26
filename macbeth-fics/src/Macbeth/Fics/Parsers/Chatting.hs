module Macbeth.Fics.Parsers.Chatting (
    says
  , tell
  , channelTell
  , told
) where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Chat
import qualified Macbeth.Fics.Parsers.Api as Api
import qualified Macbeth.Fics.Parsers.Players as P


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


told :: Parser Message
told = Told
  <$> ((Api.commandHead 107 <|> Api.commandHead 132) *> "(told " *> P.userHandle)
  <*> ((", " *> (Just <$> status)) <|> pure Nothing)


status :: Parser ChatStatus
status =
  "who is playing" *> pure Playing <|>
  (Busy <$> manyTill anyChar " (")
