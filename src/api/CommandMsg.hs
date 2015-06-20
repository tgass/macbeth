module CommandMsg (
  CommandMsg (..),
  CommandHead (..)
) where

import Api
import Challenge
import Game
import Move
import Rating
import Seek

data CommandMsg =   GameMove Move
                  | Games [Game]
                  | Observe Move

                  | StartGame Int Move
                  | MatchRequested Challenge
                  | AcceptChallenge Move
                  | DeclineChallenge
                  | DrawOffered
                  | GameResult { gameId :: Int, reason :: String, result :: GameResult }


                  | NewSeek Seek
                  | RemoveSeeks [Int]
                  | ClearSeek

                  | Login
                  | Password
                  | GuestLogin String
                  | UnkownUsername String
                  | LoggedIn String
                  | InvalidPassword
                  | Prompt
                  | SettingsDone
                  | Acknoledge
                  | TextMessage String

                  {- Internal -}
                  | NewGame Int
                  | CreatingGame GameInfo
                  | ConfirmMove Move
                  | Boxed [CommandMsg] deriving (Show)

data CommandHead = CommandHead { commandId :: Int } deriving (Show)
