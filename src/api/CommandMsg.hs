module CommandMsg (
  CommandMsg (..),
  CommandHead (..)
) where

import Api
import Game
import Seek

import qualified Data.ByteString.Char8 as BS

type Position = [(Square, Piece)]

data CommandMsg =  ObserveMsg { head :: CommandHead
                              , move :: Move }
                      | GamesMsg { head :: CommandHead
                                  , gamesList :: [Game]
                                  }
                      | SoughtMsg { head :: CommandHead
                                  , soughtList :: [Seek] }
                      | GameResultMsg { gameId :: Int
                                      , result :: GameResult
                                      }
                      | AcceptMsg { move :: Move}
                      | MatchMsg { id :: Int}
                      | AcknoledgeMessage { head :: CommandHead }
                      | MoveMsg { move :: Move}
                      | LoginMessage
                      | PasswordMessage
                      | GuestLoginMsg { name :: String }
                      | UnkownUsernameMsg { name :: String }
                      | LoggedInMessage
                      | InvalidPasswordMsg
                      | PromptMessage
                      | SettingsDoneMsg
                      | TextMessage { message :: BS.ByteString } deriving (Show)

data CommandHead = CommandHead { commandId :: Int } deriving (Show)
