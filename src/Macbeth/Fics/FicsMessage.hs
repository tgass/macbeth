module Macbeth.Fics.FicsMessage (
  FicsMessage (..)
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Player
import Macbeth.Fics.Api.Challenge
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.PendingOffer
import Macbeth.Fics.Api.Seek

import Control.Concurrent.Chan

data FicsMessage =
  -- | 1. Confirmation of a user move
  --   2. Reseted position after illegal user move
  --   3. Move by oponent
    GameMove { context :: MoveModifier, move :: Move }

  -- | Pieces holdings in Bughouse / Crazyhouse games
  | PieceHolding { gameId :: Int, phWhite :: [PType], phBlack :: [PType] }

  -- | Answer to 'observe' command (BLK_OBSERVE 80)
  | Observe Move

  -- | If id in 'observe id' does not exist
  | NoSuchGame
  | UserNotLoggedIn Username

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
  | DrawRequestDeclined Username

  -- | The oponent wants to abort.
  | AbortRequest Username
  | AbortRequestDeclined Username

  -- | The oponent wants to takeback one or more half-moves
  | TakebackRequest Username Int

  -- | Promotion piece
  | PromotionPiece PType

  | NewSeek Seek
  | RemoveSeeks [Int]
  | ClearSeek
  | SeekNotAvailable

  | PartnerNotOpen Username

  -- | Answer to 'games' command (BLK_GAMES 43)
  | Games [Game]
  | Players [Player]
  | Finger UserHandle String
  | History UserHandle String

  | Login
  | LoginTimeout
  | Password
  | GuestLogin Username
  | LoggedIn UserHandle
  | InvalidPassword
  | TextMessage String

  {- Unused -}
  | UnkownUsername Username

  {- Internal -}
  | WxClose
  | WxMatchAccepted Move (Chan FicsMessage)
  | WxObserve Move (Chan FicsMessage)
  | NullCommand
  | GameCreation Int
  | TakebackAccepted Username
  | Boxed [FicsMessage] deriving (Show, Eq)

instance Show (Chan a) where
  show _ = "Chan"

