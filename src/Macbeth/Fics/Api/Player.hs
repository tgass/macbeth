module Macbeth.Fics.Api.Player where

import Macbeth.Fics.Api.Rating hiding (None)

import Data.Ord

data Player = Player {
    rating :: Rating
  , status :: Status
  , handle :: UserHandle 
  } deriving (Eq, Show)

data Status = 
    InvolvedInAGame
  | RunningASimulMatch
  | NotOpenForMatch
  | ExaminingAGame
  | InactiveOrBusy
  | NotBusy
  | InvolvedInATournament deriving (Eq, Show, Ord)

type Username = String

data UserHandle = UserHandle {
    name :: Username
  , handleType :: [HandleType] 
  } deriving (Eq, Show)


emptyUserHandle :: UserHandle
emptyUserHandle = UserHandle "" [Unregistered]


isGuest :: UserHandle -> Bool
isGuest = elem Unregistered . handleType


instance Ord UserHandle where
  compare = comparing name


data HandleType = 
    Admin
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

