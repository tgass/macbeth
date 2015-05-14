{-# LANGUAGE OverloadedStrings #-}

module SeekMsgParsers (
  clearSeek,
  newSeek,
  removeSeeks
) where

import Api
import CommandMsg
import Seek2

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

clearSeek :: Parser CommandMsg
clearSeek = "<sc>" >> return ClearSeek

newSeek :: Parser CommandMsg
newSeek = seek' >>= return . NewSeek

removeSeeks :: Parser CommandMsg
removeSeeks = RemoveSeeks <$> ("<sr>" *> many1 (space *> decimal))

seek' :: Parser Seek2
seek' = Seek2
  <$> ("<s>" *> space *> decimal)
  <*> (space *> "w=" *> manyTill anyChar space)
  <*> ("ti=" *> manyTill anyChar space *> "rt=" *> rating')
  <*> (space *> "t=" *> decimal)
  <*> (space *> "i=" *> decimal)
  <*> (space *> "r=" *> ("r" *> pure True <|> "u" *> pure False))
  <*> (space *> "tp=" *> gameType')
  <*> (space *> "c=" *> ("W" *> pure (Just White) <|>
                         "B" *> pure (Just Black) <|>
                         "?" *> pure Nothing))
  <*> (space *> "rr=" *> ((,) <$> decimal <*> ("-" *> decimal)))

gameType' :: Parser GameType
gameType' = "untimed" *> pure Untimed <|>
            "lightning" *> pure Lightning <|>
            "blitz" *> pure Blitz <|>
            "standard" *> pure Standard <|>
            takeTill (== ' ') *> pure NonStandardGame


rating' :: Parser Rating
rating' = do
  r <- decimal
  rating <- " " *> pure (Rating r) <|> "E" *> pure (Rating r) <|> "P" *> pure Guest
  return rating

clearSeek' = BS.pack "<sc>"
newSeek' = BS.pack "<s> 7 w=GuestNMZJ ti=01 rt=0P t=15 i=5 r=u tp=standard c=W rr=0-9999 a=t f=t"
newSeek'' = BS.pack "<s> 16 w=CatNail ti=02 rt=1997  t=3 i=0 r=u tp=suicide c=? rr=0-9999 a=f f=f"
-- use: parseOnly removeSeeks removeSeeks'
removeSeeks' = BS.pack "<sr> 59 3 11"
