module Lentils.Api.CommandMsg (
  CommandMsg (..)
) where

import Lentils.Api.Api
import Lentils.Api.Challenge
import Lentils.Api.Game
import Lentils.Api.Move
import Lentils.Api.Seek

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

