module Macbeth.Api.CommandMsg (
  CommandMsg (..)
) where

import Macbeth.Api.Api
import Macbeth.Api.Challenge
import Macbeth.Api.Game
import Macbeth.Api.Move
import Macbeth.Api.Seek

data CommandMsg =   GameMove Move
                  | PieceHolding { gameId :: Int, h_white :: [PType], h_black :: [PType] }
                  | Games [Game]
                  | Observe Move
                  | RemovingObservedGame
                  | NoSuchGame

                  | MatchRequested Challenge
                  | MatchAccepted Move
                  | MatchDeclined
                  | MatchUserNotLoggedIn {user :: String}

                  | GameResult { gameId :: Int, reason :: String, result :: GameResult }

                  | PendingOffers { to :: [PendingOffer], from :: [PendingOffer] }
                  | OfferAccepted
                  | OfferDeclined
                  | IdenticalOffer

                  | DrawRequest
                  | AbortRequest String
                  | TakebackRequest String Int

                  | NewSeek Seek
                  | RemoveSeeks [Int]
                  | ClearSeek
                  | SeekNotAvailable

                  | Finger String String

                  | Login
                  | LoginTimeout
                  | Password
                  | GuestLogin String
                  | LoggedIn String
                  | InvalidPassword
                  | Prompt
                  | SettingsDone
                  | Acknoledge
                  | TextMessage String

                  {- Unused -}
                  | UnkownUsername String

                  {- Internal -}
                  | NullCommand
                  | GameCreation { gameId :: Int, description :: String }
                  | Boxed [CommandMsg] deriving (Show, Eq)

