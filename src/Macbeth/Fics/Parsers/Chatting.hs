{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Fics.Parsers.Chatting (
    says
  , tell
  , told
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Chat
import qualified Macbeth.Fics.Parsers.Api as Api
import qualified Macbeth.Fics.Parsers.Players as P


import Control.Applicative
import Data.Attoparsec.ByteString.Char8


says :: Parser FicsMessage
says = Chat <$> (Say
  <$> P.userHandle
  <*> ("[" *> Api.gameId <* "]")
  <*> (" says: " *> manyTill anyChar "\n"))


tell :: Parser FicsMessage
tell = Chat <$> (Tell
  <$> P.userHandle
  <*> (" tells you: " *> manyTill anyChar "\n"))


told :: Parser FicsMessage
told = Chat <$> (Told
  <$> ((Api.commandHead 107 <|> Api.commandHead 132) *> "(told " *> P.userHandle)
  <*> ((", " *> (Just <$> status)) <|> pure Nothing))


status :: Parser Status'
status =
  "who is playing" *> pure Playing <|>
  (Busy <$> manyTill anyChar " (")
