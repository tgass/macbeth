module Utils (
  formatTime,
  dummyMove,
  emptyMove
) where

import Api
import FicsConnection2

import Control.Monad.State

formatTime :: Int -> String
formatTime seconds = show h ++ " : " ++ format m ++ " : " ++ format s
  where
    (_, (h,m,s)) = runState ( return seconds >>= calc ) (0,0,0)


calc :: Int -> State (Int, Int, Int) Int
calc seconds
  | seconds >= 3600 = get >>= \(h, m, s) -> put (h+1, m, s) >> calc (seconds - 3600)
  | seconds >= 60 = get >>= \(h, m, s) -> put (h, m+1, s) >> calc (seconds - 60)
  | otherwise = get >>= \(h, m, s) -> put (h, m, seconds) >> return 0

format :: Int -> String
format i
  | i < 10 = "0" ++ show i
  | otherwise = show i




emptyMove id = Move {
    Api.position = [],
    turn = White,
    doublePawnPush = Nothing,
    Api.gameId = id,
    nameW = "",
    nameB = "",
    relation = MyMove,
    moveNumber = 1,
    moveVerbose = "none",
    timeTaken = "0",
    remainingTimeW = 0,
    remainingTimeB = 0,
    movePretty = "none"
  }

dummyMove = Move {
    Api.position = [ (Square A One, Piece Rook White)
                   , (Square A Two, Piece Pawn White)
                   , (Square B Two, Piece Pawn White)
                   , (Square C Two, Piece Pawn White)
                   , (Square E Eight, Piece King Black)
                   , (Square D Eight, Piece Queen Black)
                   ],
    turn = Black,
    doublePawnPush = Nothing,
    Api.gameId = 1,
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
