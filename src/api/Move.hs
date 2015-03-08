module Move (
  Move(..),
  remainingTime,
  decreaseRemainingTime,
  namePlayer,
  Relation(..),
  dummyMove
) where

import Api

data Move = Move { position :: [(Square, Piece)]
                 , turn :: Color
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
                 , movePretty :: String
                 }

instance Show Move where
  show m = "Move { gameId=" ++ show (gameId m) ++ ", nameW=" ++ nameW m ++ ", move=" ++ movePretty m ++ "}"



data Relation = MyMove | OponentsMove | Observing | Other deriving (Show, Eq)


remainingTime :: Api.Color -> Move -> Int
remainingTime Black = remainingTimeB
remainingTime White = remainingTimeW


decreaseRemainingTime :: Api.Color -> Move -> Move
decreaseRemainingTime Black move = move {remainingTimeB = remainingTimeB move - 1}
decreaseRemainingTime White move = move {remainingTimeW = remainingTimeW move - 1}


namePlayer :: Api.Color -> Move -> String
namePlayer White = nameW
namePlayer Black = nameB


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
    relation = Observing,
    moveNumber = 0,
    moveVerbose = "none",
    timeTaken = "0",
    remainingTimeW = 0,
    remainingTimeB = 0,
    movePretty = "none"
  }
