module Macbeth.Fics.Api.Player (
  Player (..),
  Status (..),
  Handle (..),
  HandleType (..)
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Rating hiding (None)

data Player = Player {
    rating :: Rating
  , status :: Status
  , handle :: Handle } deriving (Eq, Show)

data Status = InvolvedInAGame
            | RunningASimulMatch
            | NotOpenForMatch
            | ExaminingAGame
            | InactiveOrBusy
            | NotBusy
            | InvolvedInATournament deriving (Eq, Show)


data Handle = Handle {
    username :: Username
  , handleType :: HandleType }  deriving (Eq, Show)

data HandleType = None
                | Admin
                | Blindfold
                | Computer
                | Team
                | Unregistered
                | ChessAdvisor
                | ServiceRepresentative
                | TournamentDirectorOrBot
                | MamerManager
                | GrandMaster
                | InternationalMaster
                | FideMaster
                | WomenGrandMaster
                | WomenInternationalMaster
                | WomenFideMaster deriving (Eq, Show)

