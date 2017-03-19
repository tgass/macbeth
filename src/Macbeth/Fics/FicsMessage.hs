{-# LANGUAGE FlexibleInstances #-}

module Macbeth.Fics.FicsMessage (
  FicsMessage (..),
  DeclineSubject (..)
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Player
import Macbeth.Fics.Api.Chat
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.OngoingGame
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.Result
import Macbeth.Fics.Api.Seek

import Control.Concurrent.Chan

data FicsMessage =
    GameMove { context :: MoveModifier, move :: Move }
  | PieceHolding { gameId :: GameId, phWhite :: [Piece], phBlack :: [Piece] }
  | GameResult Result

  | Observing GameId GameParams
  | NewGameParamsUser GameParams
  | NewGameIdUser GameId
  | NewGameUser GameId GameParams -- | Merged

  | NoSuchGame -- | If id in 'observe id' does not exist
  | UserNotLoggedIn Username

  | MatchRequested Challenge
  | Pending PendingOffer
  | PendingRemoved Int

  | DrawRequest Username
  | AbortRequest Username
  | TakebackRequest Username Int
  | OponentDecline Username DeclineSubject

  | PromotionPiece PType

  | NewSeek Seek
  | RemoveSeeks [Int]
  | ClearSeek
  | SeekNotAvailable

  | PartnerNotOpen UserHandle
  | PartnerOffer UserHandle
  | PartnerAccepted UserHandle
  | PartnerDeclined UserHandle

  | Games [OngoingGame]
  | Players [Player]
  | Finger UserHandle String
  | History UserHandle String
  | Ping {lagMin :: Int, lagAvg :: Int, lagMax :: Int}

  | Chat ChatMsg

  | LoginPrompt
  | LoginTimeout
  | Password
  | GuestLogin Username
  | LoggedIn UserHandle
  | InvalidPassword
  | UnkownUsername Username
  | TextMessage String

  {- Internal -}
  | TakebackAccepted (Maybe Username)
  | IllegalMove String
  | WxClose
  | WxOpenBoard GameId GameParams (Chan FicsMessage) deriving (Show, Eq)


data DeclineSubject = DrawReq | TakebackReq | AbortReq | MatchReq deriving Eq


instance Show DeclineSubject where
  show DrawReq = "draw"
  show TakebackReq = "takeback"
  show AbortReq = "abort"
  show MatchReq = "match"


instance Show (Chan FicsMessage) where
  show _ = "Chan"

