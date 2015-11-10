module Macbeth.Api.CommandMsg (
  CommandMsg (..)
) where

import Macbeth.Api.Api
import Macbeth.Api.Challenge
import Macbeth.Api.Game
import Macbeth.Api.Move
import Macbeth.Api.Seek

data CommandMsg =   GameMove Move
                  | Games [Game]
                  | Observe Move
                  | RemovingObservedGame
                  | NoSuchGame

                  | MatchRequested Challenge
                  | MatchAccepted Move
                  | MatchDeclined
                  | DrawOffered
                  | DrawDeclined
                  | GameResult { gameId :: Int, reason :: String, result :: GameResult }

                  | PendingOffers { to :: [PendingOffer], from :: [PendingOffer] }
                  | OfferAccepted
                  | OfferDeclined
                  | IdenticalOffer

                  | AbortRequest
                  | AbortRequested String
                  | AbortDeclined
                  | AbortAccepted
                  | AbortedGame { gameId :: Int, reason :: String }

                  | NewSeek Seek
                  | RemoveSeeks [Int]
                  | ClearSeek

                  | Finger String String

                  | Login
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
                  | Boxed [CommandMsg] deriving (Show, Eq)

