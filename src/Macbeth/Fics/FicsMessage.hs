module Macbeth.Fics.FicsMessage (
  FicsMessage (..),
  DeclineSubject (..)
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Player
import Macbeth.Fics.Api.Challenge
import Macbeth.Fics.Api.Chat
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.PendingOffer
import Macbeth.Fics.Api.Result
import Macbeth.Fics.Api.Seek

import Control.Concurrent.Chan

data FicsMessage =
  -- | 1. Confirmation of a user move
  --   2. Reseted position after illegal user move
  --   3. Move by oponent
    GameMove { context :: MoveModifier, move :: Move }

  | IllegalMove String

  -- | Pieces holdings in Bughouse / Crazyhouse games
  | PieceHolding { gameId :: GameId, phWhite :: [PType], phBlack :: [PType] }

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

  -- | Not concering if the user or his oponent is checkmated/out of time/.. GameResult informs
  -- that the game is over.
  | GameResult Result

  | Pending PendingOffer
  | PendingRemoved Int

  -- | In-game requests
  | DrawRequest
  | AbortRequest Username
  | TakebackRequest Username Int
  | OponentDecline Username DeclineSubject


  -- | Promotion piece
  | PromotionPiece PType

  | NewSeek Seek
  | RemoveSeeks [Int]
  | ClearSeek
  | SeekNotAvailable

  | PartnerNotOpen UserHandle
  | PartnerOffer UserHandle
  | PartnerAccepted UserHandle
  | PartnerDeclined UserHandle

  -- | Answer to 'games' command (BLK_GAMES 43)
  | Games [Game]
  | Players [Player]
  | Finger UserHandle String
  | History UserHandle String
  | Ping {lagMin :: Int, lagAvg :: Int, lagMax :: Int}

  -- | Chatting
  | Chat ChatMsg

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
  | GameCreation GameId
  | Observing GameId
  | TakebackAccepted Username
  | WxClose
  | WxMatchAccepted Move (Chan FicsMessage)
  | WxObserve Move (Chan FicsMessage) deriving (Show, Eq)


data DeclineSubject = DrawReq | TakebackReq | AbortReq | MatchReq deriving Eq


instance Show DeclineSubject where
  show DrawReq = "draw"
  show TakebackReq = "takeback"
  show AbortReq = "abort"
  show MatchReq = "match"


instance Show (Chan a) where
  show _ = "Chan"

