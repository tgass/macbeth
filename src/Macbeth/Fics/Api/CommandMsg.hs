module Macbeth.Fics.Api.CommandMsg (
  CommandMsg (..)
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Challenge
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.PendingOffer
import Macbeth.Fics.Api.Seek

data CommandMsg =   GameMove { illegal :: Bool, move :: Move }
                  | PieceHolding { gameId :: Int, phWhite :: [PType], phBlack :: [PType] }
                  | Games [Game]
                  | Observe Move
                  | RemovingObservedGame
                  | NoSuchGame

                  | MatchRequested Challenge
                  | MatchAccepted Move
                  | MatchDeclined
                  | MatchUserNotLoggedIn Username

                  | GameResult { gameId :: Int, reason :: String, result :: GameResult }

                  | PendingOffers { to :: [PendingOffer], from :: [PendingOffer] }
                  | OfferAccepted
                  | OfferDeclined
                  | IdenticalOffer

                  | DrawRequest
                  | AbortRequest Username
                  | TakebackRequest Username Int

                  | NewSeek Seek
                  | RemoveSeeks [Int]
                  | ClearSeek
                  | SeekNotAvailable

                  | Finger Username String

                  | Login
                  | LoginTimeout
                  | Password
                  | GuestLogin Username
                  | LoggedIn Username
                  | InvalidPassword
                  | Prompt
                  | SettingsDone
                  | Acknoledge
                  | TextMessage String

                  {- Unused -}
                  | UnkownUsername Username

                  {- Internal -}
                  | WxClose
                  | NullCommand
                  | GameCreation { gameId :: Int, description :: String }
                  | Boxed [CommandMsg] deriving (Show, Eq)

