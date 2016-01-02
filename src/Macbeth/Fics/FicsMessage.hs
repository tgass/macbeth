module Macbeth.Fics.FicsMessage (
  FicsMessage (..)
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Challenge
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.PendingOffer
import Macbeth.Fics.Api.Seek

data FicsMessage =
  -- | 1. Confirmation of a move
  --   2. Reseted position after illegal move
  --   3. Move by oponent
    GameMove { illegal :: Bool, move :: Move }

  -- | Pieces holdings in Bughouse / Crazyhouse games
  | PieceHolding { gameId :: Int, phWhite :: [PType], phBlack :: [PType] }

  -- | Answer to 'games' command (BLK_GAMES 43)
  | Games [Game]

  -- | Answer to 'observe' command (BLK_OBSERVE 80)
  | Observe Move

  -- | If id in 'observe id' does not exist
  | NoSuchGame

  -- | Match offered by another player
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
  | Boxed [FicsMessage] deriving (Show, Eq)
