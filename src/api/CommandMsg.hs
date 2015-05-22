module CommandMsg (
  CommandMsg (..),
  CommandHead (..)
) where

import Api
import Game
import Seek2
import Move

import qualified Data.ByteString.Char8 as BS

type Position = [(Square, Piece)]

data CommandMsg =   GameMove Move
                  | Games [Game]
                  | Sought [Seek2]
                  | Observe Move

                  | StartGame { id :: Int, _move :: Move }
                  | Challenge { nameW :: String, ratingW :: Rating, nameB :: String, ratingB :: Rating, params :: String}
                  | AcceptChallenge Move
                  | DeclineChallenge
                  | DrawOffered
                  | GameResult { gameId :: Int, reason :: String, result :: GameResult }

                  | Login
                  | Password
                  | GuestLogin String
                  | UnkownUsername String
                  | LoggedIn String
                  | InvalidPassword
                  | Prompt
                  | SettingsDone
                  | Acknoledge
                  | TextMessage { message :: BS.ByteString }
                  {- Internal -}

                  | NewGame Int
                  | CreatingGame GameInfo
                  | ConfirmMove Move
                  | Boxed [CommandMsg]
                  | NewSeek { seek :: Seek2 }
                  | RemoveSeeks { ids :: [Int] }
                  | ClearSeek deriving (Show)

data CommandHead = CommandHead { commandId :: Int } deriving (Show)
