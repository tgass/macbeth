{-# LANGUAGE OverloadedStrings #-}

module CommandMsgParser (
 parseCommandMsg
) where

import Api
import CommandMsg
import GamesParser
import Move
import MoveParser2
import qualified SeekMsgParsers as SP
import qualified MatchMsgParsers as MP

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List.Split (splitOn)


parseCommandMsg :: BS.ByteString -> Either String CommandMsg
parseCommandMsg str = parseOnly parser str where
  parser = choice [ SP.clearSeek
                  , SP.newSeek
                  , SP.removeSeeks

                  , MP.gameResult
                  , MP.match'
                  , MP.challenge
                  , MP.declinedChallenge
                  , MP.drawOffered

                  , games
                  , observe
                  , accept
                  , gameResult
                  , confirmMove
                  , seekInfoBlock
                  , seekMatchesAlreadyPosted
                  , move'

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
observe = Observe <$> (commandHead 80 *> move'')

accept :: Parser CommandMsg
accept = Accept <$> (commandHead 11 *> move'')

gameResult :: Parser CommandMsg
gameResult = commandHead 103 *> MP.gameResult

confirmMove :: Parser CommandMsg
confirmMove = GameMove <$> (commandHead 1 *> move'')

seekInfoBlock :: Parser CommandMsg
seekInfoBlock = Boxed
  <$> (commandHead 56 *> "seekinfo set.\n" *> sepBy (choice [ SP.clearSeek, SP.newSeek <* takeTill (== '\n')]) "\n")

seekMatchesAlreadyPosted :: Parser CommandMsg
seekMatchesAlreadyPosted = do
  commandHead 115
  option "" "You are unregistered - setting to unrated.\n"
  rs <- "Your seek matches one already posted by" *> takeTill (== '<') *> SP.removeSeeks
  mv <- takeTill (=='<') *> move'
  return $ Boxed [rs, mv]

move' :: Parser CommandMsg
move' = GameMove <$> parseMove


{- *** LOGIN *** -}
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
acknoledge = commandHead 519 *> (char $ chr 23) *> pure Acknoledge

settingsDone :: Parser CommandMsg
settingsDone = (char $ chr 23) *> pure SettingsDone


{- HELPER -}

move'' :: Parser Move
move'' = takeTill (== '<') *> parseMove


commandHead :: Int -> Parser CommandHead
commandHead code = do
  char $ chr 21
  id <- decimal
  char $ chr 22
  string $ BS.pack $ show code
  char $ chr 22
  return $ CommandHead id


{- TEST DATA -}

creatingGame' = BS.pack "Creating: Altivolous (1086) Schoon (1013) rated blitz 5 3"
seekMatchesAlreadyPosted' = BS.pack "Your seek matches one already posted by GuestJYQC.\n\n<sr> 119\nfics% \nCreating: GuestJYQC (++++) GuestNGCB (++++) unrated blitz 2 12\n{Game 364 (GuestJYQC vs. GuestNGCB) Creating unrated blitz match.}\n\a\n<12> rnbqkbnr pppppppp -------- -------- -------- -------- PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 364 GuestJYQC GuestNGCB -1 2 12 39 39 120 120 1 none (0:00) none 1 0 0\n"
seekMatchesAlreadyPosted'' = BS.pack "You are unregistered - setting to unrated.\nYour seek matches one already posted by GuestJYQC.\n\n<sr> 119\nfics% \nCreating: GuestJYQC (++++) GuestNGCB (++++) unrated blitz 2 12\n{Game 364 (GuestJYQC vs. GuestNGCB) Creating unrated blitz match.}\n\a\n<12> rnbqkbnr pppppppp -------- -------- -------- -------- PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 364 GuestJYQC GuestNGCB -1 2 12 39 39 120 120 1 none (0:00) none 1 0 0\n"
seekInfoBlock' = BS.pack "seekinfo set.\n<sc>\n<s> 16 w=CatNail ti=02 rt=1997  t=3 i=0 r=u tp=suicide c=? rr=0-9999 a=f f=f\n<s> 44 w=masheen ti=02 rt=2628  t=5 i=0 r=u tp=suicide c=? rr=0-9999 a=t f=f\n<s> 51 w=masheen ti=02 rt=2628  t=2 i=12 r=u tp=suicide c=? rr=0-9999 a=t f=f\n<s> 81 w=GuestHZLT ti=01 rt=0P t=2 i=0 r=u tp=lightning c=? rr=0-9999 a=t f=f\n"
playMsg = BS.pack "Creating: GuestCCFP (++++) GuestGVJK (++++) unrated blitz 0 20 {Game 132 (GuestCCFP vs. GuestGVJK) Creating unrated blitz match.} <12> rnbqkbnr pppppppp ———— ———— ———— ———— PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 132 GuestCCFP GuestGVJK -1 0 20 39 39 10 10 1 none (0:00) none 1 0 0"
obs = BS.pack "You are now observing game 157.Game 157: IMUrkedal (2517) GMRomanov (2638) unrated standard 120 0<12> -------- -pp-Q--- pk------ ----p--- -P---p-- --qB---- -------- ---R-K-- B -1 0 0 0 0 9 157 IMUrkedal GMRomanov 0 120 0 18 14 383 38 57 K/e1-f1 (0:03) Kf1 0 0 0"
guestLoginTxt = BS.pack $ "Press return to enter the server as \"FOOBAR\":"
