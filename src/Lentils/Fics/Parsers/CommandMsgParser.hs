{-# LANGUAGE OverloadedStrings #-}

module Lentils.Fics.Parsers.CommandMsgParser (
 parseCommandMsg
) where

import Lentils.Api.Challenge
import Lentils.Api.CommandMsg
import Lentils.Api.Game
import Lentils.Fics.Parsers.GamesParser
import Lentils.Fics.Parsers.MoveParser2
import Lentils.Fics.Parsers.RatingParser
import qualified Lentils.Fics.Parsers.SeekMsgParsers as SP

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Char
import Data.List.Split (splitOn)
import qualified Data.ByteString.Char8 as BS



parseCommandMsg :: BS.ByteString -> Either String CommandMsg
parseCommandMsg = parseOnly parser where
  parser = choice [ SP.clearSeek
                  , SP.newSeek
                  , SP.removeSeeks

                  , matchRequested
                  , matchUpdated
                  , declinedChallenge
                  , drawOffered
                  , drawDeclined

                  , games
                  , playSeek
                  , observe
                  , noSuchGame
                  , accept
                  , gameResult
                  , gameResult'
                  , gameResultMutualDraw
                  , gameResultAcceptDraw

                  , confirmGameMove
                  , seekInfoBlock
                  , seekMatchesAlreadyPosted
                  , gameMove

                  , login
                  , password
                  , guestLogin
                  , unknownUsername
                  , loggedIn
                  , invalidPassword
                  , prompt
                  , acknoledge
                  , settingsDone
                  ]

games :: Parser CommandMsg
games = Games <$> (commandHead 43 *> paresGamesList)

observe :: Parser CommandMsg
observe = Observe <$> (commandHead 80 *> move)

noSuchGame :: Parser CommandMsg
noSuchGame = commandHead 80 *> "There is no such game." *> pure NoSuchGame

confirmGameMove :: Parser CommandMsg
confirmGameMove = GameMove <$> (commandHead 1 *> move)

gameMove :: Parser CommandMsg
gameMove = GameMove <$> move

playSeek :: Parser CommandMsg
playSeek = do
  commandHead 158
  rs <- "\n" *> SP.removeSeeks <* "\n"
  mv <- takeTill (=='<') *> (GameMove <$> move)
  return $ Boxed [rs, mv]

matchRequested :: Parser CommandMsg
matchRequested = MatchRequested <$> (Challenge
  <$> ("Challenge: " *> manyTill anyChar space)
  <*> ("(" *> rating)
  <*> (") " *> manyTill anyChar space)
  <*> ("(" *> rating)
  <*> (") " *> manyTill anyChar ".")) --unrated blitz 2 12."

matchUpdated :: Parser CommandMsg
matchUpdated = MatchUpdated <$> manyTill anyChar space <* "updates the match request."

accept :: Parser CommandMsg
accept = MatchAccepted <$> (commandHead 11 *> move)

declinedChallenge :: Parser CommandMsg
declinedChallenge = "\"" *> manyTill anyChar "\" declines the match offer." *> pure MatchDeclined

seekInfoBlock :: Parser CommandMsg
seekInfoBlock = Boxed
  <$> (commandHead 56 *> "seekinfo set.\n" *> sepBy (choice [ SP.clearSeek, SP.newSeek <* takeTill (== '\n')]) "\n")

seekMatchesAlreadyPosted :: Parser CommandMsg
seekMatchesAlreadyPosted = do
  commandHead 155
  option "" "You are unregistered - setting to unrated.\n"
  rs <- "Your seek matches one already posted by" *> takeTill (== '<') *> SP.removeSeeks <* "\n"
  mv <- takeTill (=='<') *> (GameMove <$> move)
  return $ Boxed [rs, mv]

drawOffered :: Parser CommandMsg
drawOffered = manyTill anyChar space *> "offers you a draw." *> pure DrawOffered

drawDeclined :: Parser CommandMsg
drawDeclined = manyTill anyChar space *> "declines the draw request." *> pure DrawDeclined

gameResult :: Parser CommandMsg
gameResult = commandHead 103 *> gameResult'

gameResultMutualDraw :: Parser CommandMsg
gameResultMutualDraw = commandHead 34 *> gameResult'

gameResultAcceptDraw :: Parser CommandMsg
gameResultAcceptDraw = commandHead 11 *> takeTill (== '{') *> gameResult'

gameResult' :: Parser CommandMsg
gameResult' = GameResult
  <$> (skipSpace *> "{Game" *> space *> decimal)
  <*> (takeTill (== ')') *> ") " *> manyTill anyChar "} ")
  <*> ("1-0" *> pure WhiteWins <|> "0-1" *> pure BlackWins <|>  "1/2-1/2" *> pure Draw)

login :: Parser CommandMsg
login = "login: " *> pure Login

password :: Parser CommandMsg
password = "password: " *> pure Password

guestLogin :: Parser CommandMsg
guestLogin = GuestLogin <$> ("Press return to enter the server as \"" *> manyTill anyChar "\":")

unknownUsername :: Parser CommandMsg
unknownUsername = UnkownUsername <$> ("\"" *> manyTill anyChar "\" is not a registered name.")

-- | Beware the guest handles: ie GuestXWLW(U)
loggedIn :: Parser CommandMsg
loggedIn = LoggedIn
  <$> ("**** Starting FICS session as " *> (Prelude.head . splitOn "(") `fmap` manyTill anyChar " ****")

invalidPassword :: Parser CommandMsg
invalidPassword = "**** Invalid password! ****" *> pure InvalidPassword

prompt :: Parser CommandMsg
prompt = "fics% " *> pure Prompt

acknoledge :: Parser CommandMsg
acknoledge = commandHead 519 *> char (chr 23) *> pure Acknoledge

settingsDone :: Parser CommandMsg
settingsDone = char (chr 23) *> pure SettingsDone


{- HELPER -}
data CommandHead = CommandHead Int deriving (Show)

commandHead :: Int -> Parser CommandHead
commandHead code = do
  char $ chr 21
  id <- decimal
  char $ chr 22
  string $ BS.pack $ show code
  char $ chr 22
  return $ CommandHead id


{- TEST DATA -}
seekMatchesAlreadyPosted' = BS.pack "\NAK4\SYN155\SYNYour seek matches one already posted by GuestJYQC.\n\n<sr> 119\nfics% \nCreating: GuestJYQC (++++) GuestNGCB (++++) unrated blitz 2 12\n{Game 364 (GuestJYQC vs. GuestNGCB) Creating unrated blitz match.}\n\a\n<12> rnbqkbnr pppppppp -------- -------- -------- -------- PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 364 GuestJYQC GuestNGCB -1 2 12 39 39 120 120 1 none (0:00) none 1 0 0\n"
seekMatchesAlreadyPosted'' = BS.pack "\NAK4\SYN155\SYNYou are unregistered - setting to unrated.\nYour seek matches one already posted by GuestLQFZ.\n\n<sr> 130\nfics% \nCreating: GuestLQFZ (++++) GuestSFKS (++++) unrated blitz 5 0\n{Game 214 (GuestLQFZ vs. GuestSFKS) Creating unrated blitz match.}\n\a\n<12> rnbqkbnr pppppppp -------- -------- -------- -------- PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 214 GuestLQFZ GuestSFKS -1 5 0 39 39 300 300 1 none (0:00) none 1 0 0\n\nGame 214: A disconnection will be considered a forfeit.\n\ETB"
playSeek' = BS.pack "\NAK4\SYN158\SYN\n<sr> 25\nfics% \nCreating: chicapucp (1658) GuestFTYL (++++) unrated blitz 3 0\n{Game 18 (chicapucp vs. GuestFTYL) Creating unrated blitz match.}\n\a\n<12> rnbqkbnr pppppppp -------- -------- -------- -------- PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 18 chicapucp GuestFTYL -1 3 0 39 39 180 180 1 none (0:00) none 1 0 0\n\nGame 18: A disconnection will be considered a forfeit.\n\ETB"
seekInfoBlock' = BS.pack "seekinfo set.\n<sc>\n<s> 16 w=CatNail ti=02 rt=1997  t=3 i=0 r=u tp=suicide c=? rr=0-9999 a=f f=f\n<s> 44 w=masheen ti=02 rt=2628  t=5 i=0 r=u tp=suicide c=? rr=0-9999 a=t f=f\n<s> 51 w=masheen ti=02 rt=2628  t=2 i=12 r=u tp=suicide c=? rr=0-9999 a=t f=f\n<s> 81 w=GuestHZLT ti=01 rt=0P t=2 i=0 r=u tp=lightning c=? rr=0-9999 a=t f=f\n"
playMsg = BS.pack "Creating: GuestCCFP (++++) GuestGVJK (++++) unrated blitz 0 20 {Game 132 (GuestCCFP vs. GuestGVJK) Creating unrated blitz match.} <12> rnbqkbnr pppppppp ———— ———— ———— ———— PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 132 GuestCCFP GuestGVJK -1 0 20 39 39 10 10 1 none (0:00) none 1 0 0"
obs = BS.pack "You are now observing game 157.Game 157: IMUrkedal (2517) GMRomanov (2638) unrated standard 120 0<12> -------- -pp-Q--- pk------ ----p--- -P---p-- --qB---- -------- ---R-K-- B -1 0 0 0 0 9 157 IMUrkedal GMRomanov 0 120 0 18 14 383 38 57 K/e1-f1 (0:03) Kf1 0 0 0"
guestLoginTxt = BS.pack "Press return to enter the server as \"FOOBAR\":"

