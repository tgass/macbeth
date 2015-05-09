{-# LANGUAGE OverloadedStrings #-}

module CommandMsgParser (
 parseCommandMsg
) where

import Api
import CommandMsg
import GamesParser
import Move
import MoveParser2
import SeekParser
import SeekMsgParsers

import qualified MatchMsgParsers as MP

import Control.Applicative ((*>), (<|>), pure)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List.Split (splitOn)


parseCommandMsg :: BS.ByteString -> Either String CommandMsg
parseCommandMsg str = parseOnly parser str where
  parser = choice [ sought
                  , clearSeek
                  , games
                  , observe
                  , accept
                  , playSuccess
                  , move'
                  , confirmMove
                  , MP.challenge
                  , MP.match'
                  , MP.gameResult
                  , gameResult
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

sought :: Parser CommandMsg
sought = commandHead 157 >> soughtList' >>= return . Sought

games :: Parser CommandMsg
games = commandHead 43 >> paresGamesList >>= return . Games

observe :: Parser CommandMsg
observe = commandHead 80 >> move'' >>= return . Observe

accept :: Parser CommandMsg
accept = commandHead 11 >> move'' >>= return . Accept

gameResult :: Parser CommandMsg
gameResult = commandHead 103 >> MP.gameResult

playSuccess :: Parser CommandMsg
playSuccess = commandHead 1111111 >> move'' >>= return . PlaySuccess

confirmMove :: Parser CommandMsg
confirmMove = commandHead 1 >> move'' >>= return . ConfirmMove

move' :: Parser CommandMsg
move' = parseMove >>= return . CommandMsg.Move

login :: Parser CommandMsg
login = "login: " >> return Login

password = "password: " >> return Password

guestLogin = "Press return to enter the server as \"" >>
  manyTill anyChar "\":" >>=
  return . GuestLogin

unknownUsername = "\"" >>
  manyTill anyChar "\" is not a registered name." >>=
  return . UnkownUsername

loggedIn = "**** Starting FICS session as " >>
  manyTill anyChar " ****" >>=
  return . LoggedIn . Prelude.head . splitOn "(" -- | Beware the guest handles: ie GuestXWLW(U)

invalidPassword = "**** Invalid password! ****" >> return InvalidPassword

prompt = "fics% " >> return Prompt

acknoledge = commandHead 519 >> (char $ chr 23) >> return Acknoledge

settingsDone = (char $ chr 23) >> return SettingsDone


{- HELPER -}

move'' :: Parser Move
move'' = takeTill (== '<') >> parseMove >>= return


commandHead :: Int -> Parser CommandHead
commandHead code = do
  char $ chr 21
  id <- decimal
  char $ chr 22
  string $ BS.pack $ show code
  char $ chr 22
  return $ CommandHead id


{- TEST DATA -}

playMsg = BS.pack "Creating: GuestCCFP (++++) GuestGVJK (++++) unrated blitz 0 20 {Game 132 (GuestCCFP vs. GuestGVJK) Creating unrated blitz match.} <12> rnbqkbnr pppppppp ———— ———— ———— ———— PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 132 GuestCCFP GuestGVJK -1 0 20 39 39 10 10 1 none (0:00) none 1 0 0"
obs = BS.pack "You are now observing game 157.Game 157: IMUrkedal (2517) GMRomanov (2638) unrated standard 120 0<12> -------- -pp-Q--- pk------ ----p--- -P---p-- --qB---- -------- ---R-K-- B -1 0 0 0 0 9 157 IMUrkedal GMRomanov 0 120 0 18 14 383 38 57 K/e1-f1 (0:03) Kf1 0 0 0"
guestLoginTxt = BS.pack $ "Press return to enter the server as \"FOOBAR\":"
