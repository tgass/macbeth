module Macbeth.Fics.Api.Move (
  Move(..),
  Relation(..),
  Castling(..),
  MoveModifier(..),
  remainingTime,
  decreaseRemainingTime,
  colorUser,
  namePlayer,
  isNextMoveUser,
  isOponentMove,
  wasOponentMove,
  playerColor,
  isCheck,
  isCheckmate,
  isDrop,
  isCapture,
  isCastling,
  initMove
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Player
import qualified Macbeth.Fics.Api.Game as G


data Move = Move {
    positionRaw :: String
  , position :: [(Square, Piece)]
  , turn :: PColor
  , doublePawnPush :: Maybe Column
  , castlingAv :: [Castling]
  , ply :: Int
  , gameId :: G.GameId
  , nameW :: String
  , nameB :: String
  , relation :: Relation
  , initialTime :: Int
  , incPerMove :: Int
  , whiteRelStrength :: Int
  , blackRelStrength :: Int
  , remainingTimeW :: Int
  , remainingTimeB :: Int
  , moveNumber :: Int
  , moveVerbose :: Maybe MoveDetailed
  , timeTaken :: String
  , movePretty :: Maybe String } deriving (Eq, Show)


data Relation = IsolatedPosition | ObservingExaminedGame | Examiner | MyMove | OponentsMove | Observing deriving (Show, Eq)


data Castling = WhiteLong | WhiteShort | BlackLong | BlackShort deriving (Show, Eq)


data MoveModifier = None | Illegal String | Takeback (Maybe Username) deriving (Eq)


instance Show MoveModifier where
  show (Illegal move') = "Illegal move (" ++ move' ++ ")."
  show (Takeback (Just username)) = username ++ " accepts the takeback request."
  show (Takeback Nothing) = ""
  show None = ""


remainingTime :: PColor -> Move -> Int
remainingTime Black = remainingTimeB
remainingTime White = remainingTimeW


decreaseRemainingTime :: PColor -> Move -> Move
decreaseRemainingTime Black move = move {remainingTimeB = max 0 $ remainingTimeB move - 1}
decreaseRemainingTime White move = move {remainingTimeW = max 0 $ remainingTimeW move - 1}


colorUser :: Move -> PColor
colorUser m = if relation m == MyMove then turn m else invert $ turn m


isNextMoveUser :: Move -> Bool
isNextMoveUser m = relation m == MyMove


isOponentMove :: Move -> Bool
isOponentMove m = relation m == OponentsMove


wasOponentMove :: Move -> Bool
wasOponentMove m = colorUser m == turn m


namePlayer :: PColor -> Move -> String
namePlayer White = nameW
namePlayer Black = nameB


playerColor :: String -> Move -> PColor
playerColor name' move = if nameW move == name' then White else Black


isCheck :: Move -> Bool
isCheck = maybe False ((== '+') . last) . movePretty


isCheckmate :: Move -> Bool
isCheckmate = maybe False ((== '#') . last) . movePretty


isDrop :: Move -> Bool
isDrop = maybe False (elem '@') . movePretty


isCapture :: Move -> Bool
isCapture = maybe False (elem 'x') . movePretty


isCastling :: Move -> Bool
isCastling = maybe False (elem 'O') . movePretty


initMove :: G.GameProperties -> Move
initMove gameProperties' = Move {
    positionRaw = ""
  , position = []
  , turn = White
  , doublePawnPush = Nothing
  , castlingAv = []
  , ply = 0
  , gameId = G.gameId' gameProperties'
  , nameW = G.playerW' gameProperties'
  , nameB = G.playerB' gameProperties'
  , relation = MyMove
  , moveNumber = 0
  , moveVerbose = Nothing
  , timeTaken = "0"
  , remainingTimeW = 0
  , remainingTimeB = 0
  , movePretty = Nothing
  , initialTime = 0
  , incPerMove = 0
  , whiteRelStrength = 0
  , blackRelStrength = 0 }
