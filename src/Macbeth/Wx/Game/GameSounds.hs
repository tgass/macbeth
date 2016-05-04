module Macbeth.Wx.Game.GameSounds (
  gameSounds
) where

import Macbeth.Fics.Api.Move
import Macbeth.Fics.FicsMessage
import Macbeth.OSX.Sounds
import qualified Macbeth.Wx.UserConfig as C


gameSounds :: C.Config -> Move -> FicsMessage -> IO ()
gameSounds config move msg = playSound config sound
  where sound = C.sounds config >>= findSound move msg


findSound :: Move -> FicsMessage -> C.Sounds -> Maybe String
findSound initMove msg config = case msg of
  GameMove Illegal _ -> C.illegal $ C.move $ C.game config
  GameMove _ move
    | not (C.enabledObservedGames config) && not (isGameUser move) -> Nothing
    | isCheck move -> C.check $ C.move $ C.game config
    | isCapture move -> C.capture $ C.move $ C.game config
    | isCastling move -> C.castling $ C.move $ C.game config
    | isDrop move -> C.pieceDrop $ C.move $ C.game config
    | otherwise -> C.normal $ C.move $ C.game config
  GameResult _ _ _ _ _ -> undefined
  _ -> Nothing


{-

moveSound :: MoveModifier -> Move -> C.GameS -> Maybe String
moveSound _ move

--  | userWins move = C.youWin . C.endOfGame
--  | userLoses move = C.youLose . C.endOfGame
--  | isCheckmake move = C.checkmate . C.endOfGame

  GameResult _ _ Draw -> C.draw $ C.endOfGame $ C.game s
  GameResult _ _ Aborted -> C.draw $ C.endOfGame $ C.game s
  GameResult {} -> Nothing -- is covered in moveSound
-}
