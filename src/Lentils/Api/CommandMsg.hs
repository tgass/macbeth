module Lentils.Api.CommandMsg (
  CommandMsg (..)
) where

import Lentils.Api.Challenge
import Lentils.Api.Game
import Lentils.Api.Move
import Lentils.Api.Seek

data CommandMsg =   GameMove Move
                  | Games [Game]
                  | Observe Move
                  | NoSuchGame

                  | MatchRequested Challenge
                  | MatchUpdated String
                  | MatchAccepted Move
                  | MatchDeclined
                  | DrawOffered
                  | DrawDeclined
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
                  | Boxed [CommandMsg] deriving (Show)
