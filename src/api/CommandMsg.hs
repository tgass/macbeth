module CommandMsg (
  CommandMsg (..),
  CommandHead (..)
) where

import Api
import Game
import Move
import Rating
import Seek

data CommandMsg =   GameMove Move
                  | Games [Game]
                  | Observe Move

                  | StartGame { id :: Int, _move :: Move }
                  | Challenge { nameW :: String, ratingW :: Rating, nameB :: String, ratingB :: Rating, params :: String}
                  | AcceptChallenge Move
                  | DeclineChallenge
                  | DrawOffered
                  | GameResult { gameId :: Int, reason :: String, result :: GameResult }


                  | NewSeek { seek :: Seek }
                  | RemoveSeeks { ids :: [Int] }
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
