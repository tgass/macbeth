module Macbeth.Fics.Api.Move (
  Move(..),
  Relation(..),
  Castling(..),
  MoveModifier(..),
  remainingTime,
  decreaseRemainingTime,
  nameUser,
  colorUser,
  namePlayer,
  isGameUser,
  isNextMoveUser,
  isNewGameUser,
  isOponentMove,
  wasOponentMove,
  playerColor,
  nameOponent,
  isCheckmate,
  toGameResultTuple,
  dummyMove
) where

import Macbeth.Fics.Api.Api
import qualified Macbeth.Fics.Api.Game as Game

import Data.Maybe

data Move = Move {
    positionRaw :: String
  , position :: [(Square, Piece)]
  , turn :: PColor
  , doublePawnPush :: Maybe Column
  , castlingAv :: [Castling]
  , ply :: Int
  , gameId :: Int
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
  , movePretty :: Maybe String
  } deriving (Eq, Show)

data Relation = IsolatedPosition | ObservingExaminedGame | Examiner | MyMove | OponentsMove | Observing deriving (Show, Eq)

data Castling = WhiteLong | WhiteShort | BlackLong | BlackShort deriving (Show, Eq)

data MoveModifier = None | Illegal | Takeback (Maybe Username) deriving (Eq)

instance Show MoveModifier where
  show Illegal = "Illegal move."
  show (Takeback (Just username)) = username ++ " accepts the takeback request."
  show (Takeback Nothing) = ""
  show None = ""

remainingTime :: PColor -> Move -> Int
remainingTime Black = remainingTimeB
remainingTime White = remainingTimeW


decreaseRemainingTime :: PColor -> Move -> Move
decreaseRemainingTime Black move = move {remainingTimeB = max 0 $ remainingTimeB move - 1}
decreaseRemainingTime White move = move {remainingTimeW = max 0 $ remainingTimeW move - 1}


nameUser :: Move -> String
nameUser m = namePlayer (colorUser m) m


colorUser :: Move -> PColor
colorUser m = if relation m == MyMove then turn m else invert $ turn m


isGameUser :: Move -> Bool
isGameUser m = relation m `elem` [MyMove, OponentsMove]


isNextMoveUser :: Move -> Bool
isNextMoveUser m = relation m == MyMove


isNewGameUser :: Move -> Bool
isNewGameUser m = isGameUser m && isNothing (movePretty m)


isOponentMove :: Move -> Bool
isOponentMove m = relation m == OponentsMove


wasOponentMove :: Move -> Bool
wasOponentMove m = colorUser m == turn m


namePlayer :: PColor -> Move -> String
namePlayer White = nameW
namePlayer Black = nameB


nameOponent :: Move -> String
nameOponent m = namePlayer (invert $ colorUser m) m


playerColor :: String -> Move -> PColor
playerColor name move
  | nameW move == name = White
  | otherwise = Black


isCheckmate :: Move -> Bool
isCheckmate = maybe False ((== '#') . last) . movePretty


toGameResultTuple :: Move -> (Int, String, Game.GameResult)
toGameResultTuple move = (gameId move, namePlayer colorTurn move ++ " checkmated", turnToGameResult colorTurn)
  where
    colorTurn = turn move
    turnToGameResult Black = Game.WhiteWins
    turnToGameResult White = Game.BlackWins


dummyMove :: Move
dummyMove = Move {
    positionRaw = "",
    position = [ (Square A One, Piece Rook White)
                   , (Square A Two, Piece Pawn White)
                   , (Square B Two, Piece Pawn White)
                   , (Square C Two, Piece Pawn White)
                   , (Square E Eight, Piece King Black)
                   , (Square D Eight, Piece Queen Black)
                   ],
    turn = Black,
    doublePawnPush = Nothing,
    castlingAv = [],
    ply = 0,
    gameId = 1,
    nameW = "foobar",
    nameB = "barbaz",
    relation = MyMove,
    moveNumber = 0,
    moveVerbose = Nothing,
    timeTaken = "0",
    remainingTimeW = 0,
    remainingTimeB = 0,
    movePretty = Just "f2"
  , initialTime = 300
  , incPerMove = 0
  , whiteRelStrength = 120
  , blackRelStrength = 120
  }
