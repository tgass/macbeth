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

data CommandMsg =   Games { gamesList :: [Game] }
                  | Sought { seekList :: [Seek2] }
                  | Observe { move :: Move }
                  | Accept { move :: Move}
                  | PlaySuccess { move :: Move }
                  | Challenge { nameW :: String, ratingW :: Rating, nameB :: String, ratingB :: Rating, params :: String}
                  | MatchDeclined
                  | DrawOffered
                  | Match { id :: Int }
                  | Move { move :: Move}
                  | ConfirmMove { move :: Move }
                  | GameResult { gameId :: Int, reason :: String, result :: GameResult }
                  | Login
                  | Password
                  | GuestLogin { name :: String }
                  | UnkownUsername { name :: String }
                  | LoggedIn { name :: String }
                  | InvalidPassword
                  | Prompt
                  | SettingsDone
                  | Acknoledge
                  | TextMessage { message :: BS.ByteString }
                  {- Internal -}
                  | Boxed [CommandMsg]
                  | NewSeek { seek :: Seek2 }
                  | RemoveSeeks { ids :: [Int] }
                  | ClearSeek deriving (Show)

data CommandHead = CommandHead { commandId :: Int } deriving (Show)
