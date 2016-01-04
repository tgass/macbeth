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
  -- | 1. Confirmation of a user move
  --   2. Reseted position after illegal user move
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

  -- | Starts a new game. Let it be after 'seek' or 'match' or resuming a pending game.
  -- Indifferent of whether the user or his oponent started the interaction.
  | MatchAccepted Move
  | MatchDeclined Username
  | MatchUserNotLoggedIn Username
  -- | The user made the same match offer twice
  | MatchOfferIdentical


  -- | Not concering if the user or his oponent is checkmated/out of time/.. GameResult informs
  -- that the game is over.
  | GameResult { gameId :: Int, reason :: String, result :: GameResult }

  | PendingOffers { to :: [PendingOffer], from :: [PendingOffer] }

  -- | The oponents wants to draw.
  | DrawRequest

  -- | The oponent wants to abort.
  | AbortRequest Username

  -- | The oponent wants to takeback one or more half-moves
  | TakebackRequest Username Int
  | TakebackAccepted Username

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
  | SettingsDone
  | TextMessage String

  {- Unused -}
  | UnkownUsername Username
  | Prompt
  | Acknoledge

  {- Internal -}
  | WxClose
  | NullCommand
  | GameCreation { gameId :: Int, description :: String }
  | Boxed [FicsMessage] deriving (Show, Eq)
