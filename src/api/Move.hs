module Move (
  Move(..),
  Relation(..),
  remainingTime,
  decreaseRemainingTime,
  namePlayer,
  colorOfPlayer,
  nameOponent,
  isPlayersGame,
  isPlayersNewGame,
  playerColor,
  isCheckmate,
  toGameResultTuple,
  dummyMove
) where

import Api
import qualified Game
import Data.Maybe (isNothing, fromMaybe)

data Move = Move {
    position :: [(Square, Piece)]
  , turn :: PColor
  , doublePawnPush :: Maybe Int
  , gameId :: Int
  , nameW :: String
  , nameB :: String
  , relation :: Relation
  , moveNumber :: Int
  , moveVerbose :: String
  , timeTaken :: String
  , remainingTimeW :: Int
  , remainingTimeB :: Int
  , movePretty :: Maybe String
  } deriving (Eq)

instance Show Move where
  show m = "Move { gameId=" ++ show (gameId m) ++ ", move=" ++ show (movePretty m) ++ "}"

data Relation = MyMove | OponentsMove | Observing | Other deriving (Show, Eq)


remainingTime :: Api.PColor -> Move -> Int
remainingTime Black = remainingTimeB
remainingTime White = remainingTimeW


decreaseRemainingTime :: Api.PColor -> Move -> Move
decreaseRemainingTime Black move = move {remainingTimeB = max 0 $ remainingTimeB move - 1}
decreaseRemainingTime White move = move {remainingTimeW = max 0 $ remainingTimeW move - 1}


namePlayer :: Api.PColor -> Move -> String
namePlayer White = nameW
namePlayer Black = nameB


colorOfPlayer :: Move -> Api.PColor
colorOfPlayer m = if relation m == MyMove then turn m else Api.invert $ turn m


nameOponent :: Api.PColor -> Move -> String
nameOponent White = nameB
nameOponent Black = nameW


isPlayersGame :: Move -> Bool
isPlayersGame m = (relation m == MyMove) || (relation m == OponentsMove)


isPlayersNewGame :: Move -> Bool
isPlayersNewGame m = isPlayersGame m && isNothing (movePretty m)


playerColor :: String -> Move -> Api.PColor
playerColor name move
  | Move.nameW move == name = White
  | otherwise = Black


isCheckmate :: Move -> Bool
isCheckmate = maybe False ((== '#') . last) . movePretty

toGameResultTuple :: Move -> (Int, String, Game.GameResult)
toGameResultTuple move = (Move.gameId move, namePlayer colorTurn move ++ " checkmated", turnToGameResult colorTurn)
  where
    colorTurn = turn move
    turnToGameResult Black = Game.WhiteWins
    turnToGameResult White = Game.BlackWins

dummyMove :: Move
dummyMove = Move {
    position = [ (Square A One, Piece Rook White)
                   , (Square A Two, Piece Pawn White)
                   , (Square B Two, Piece Pawn White)
                   , (Square C Two, Piece Pawn White)
                   , (Square E Eight, Piece King Black)
                   , (Square D Eight, Piece Queen Black)
                   ],
    turn = Black,
    doublePawnPush = Nothing,
    gameId = 1,
    nameW = "foobar",
    nameB = "barbaz",
    relation = MyMove,
    moveNumber = 0,
    moveVerbose = "none",
    timeTaken = "0",
    remainingTimeW = 0,
    remainingTimeB = 0,
    movePretty = Just "f2"
  }
