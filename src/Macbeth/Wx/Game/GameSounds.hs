module Macbeth.Wx.Game.GameSounds (
  gameSounds
) where

import Macbeth.Fics.Api.Move
import Macbeth.Fics.FicsMessage
import Macbeth.OSX.Sounds
import qualified Macbeth.Wx.UserConfig as C

gameSounds :: C.Config -> Move -> FicsMessage -> IO ()
gameSounds config move = sounds config (findSound move)

findSound :: Move -> FicsMessage -> Maybe String
findSound initMove msg = undefined


{-
moveSound :: MoveModifier -> Move -> C.GameS -> Maybe String
moveSound Illegal _ = C.illegal . C.move
moveSound _ move
  | isCheck move = C.check . C.move
  | isCapture move = C.capture . C.move
  | isCastling move = C.castling . C.move
  | isDrop move = C.pieceDrop . C.move
--  | userWins move = C.youWin . C.endOfGame
--  | userLoses move = C.youLose . C.endOfGame
--  | isCheckmake move = C.checkmate . C.endOfGame
  | otherwise = C.normal . C.move

  GameResult _ _ Draw -> C.draw $ C.endOfGame $ C.game s
  GameResult _ _ Aborted -> C.draw $ C.endOfGame $ C.game s
  GameResult {} -> Nothing -- is covered in moveSound

  GameMove mod move
    | not (C.enableObservedGames s) && not (isGameUser move) -> Nothing
    | otherwise -> moveSound mod move (C.game s) -}
