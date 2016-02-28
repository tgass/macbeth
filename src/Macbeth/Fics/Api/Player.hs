module Macbeth.Fics.Api.Player (
  Player (..),
  Status (..),
  UserHandle (..),
  HandleType (..)
) where

import Macbeth.Fics.Api.Rating hiding (None)

data Player = Player {
    rating :: Rating
  , status :: Status
  , handle :: UserHandle } deriving (Eq, Show)

data Status = InvolvedInAGame
            | RunningASimulMatch
            | NotOpenForMatch
            | ExaminingAGame
            | InactiveOrBusy
            | NotBusy
            | InvolvedInATournament deriving (Eq, Show)


data UserHandle = UserHandle {
    name :: String
  , handleType :: [HandleType] } deriving (Eq, Show)


data HandleType = Admin
                | Blindfold
                | Computer
                | NOT_DOCUMENTED -- ^ (D) is not documented
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

