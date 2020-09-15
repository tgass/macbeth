module Macbeth.Fics.Message where

import Control.Concurrent.Chan
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Player
import Macbeth.Fics.Api.Chat
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.Offer
import Macbeth.Fics.Api.OngoingGame
import Macbeth.Fics.Api.Result
import Macbeth.Fics.Api.Seek
import Macbeth.Fics.Api.Stored

data Message =
    GameMove { context :: !MoveModifier, move :: !Move }
  | PieceHolding { gameId :: !GameId, phWhite :: ![Piece], phBlack :: ![Piece] }
  | GameResult !Result

  | Observing GameId !GameParams
  | NewGameParamsUser !GameParams
  | NewGameIdUser !GameId
  | NewGameUser !GameId !GameParams -- | Merged

  | NoSuchGame -- | If id in 'observe id' does not exist
  | UserNotLoggedIn Username

  | MatchRequested !Challenge
  | Pending PendingOffer
  | PendingRemoved Int

  | DrawRequest Username
  | AbortRequest Username
  | TakebackRequest Username Int
  | OponentDecline Username OfferSubject

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
  | StoredGames [Stored]
  | Finger UserHandle String
  | History UserHandle String
  | Ping {lagMin :: Int, lagAvg :: Int, lagMax :: Int}

  | Chat ChatMsg

  | LoginPrompt
  | LoginTimeout
  | Password
  | GuestLogin Username
  | LoggedIn UserHandle
  | LogOut
  | AbusiveBehavior
  | InvalidPassword
  | UnkownUsername Username
  | TextMessage String

  {- Internal -}
  | TakebackAccepted (Maybe Username)
  | IllegalMove String
  | ConnectionClosed String
  | WxClose
  | WxOpenBoard GameId GameParams (Chan Message) deriving (Show, Eq)


instance Show (Chan Message) where
  show _ = "Chan"

