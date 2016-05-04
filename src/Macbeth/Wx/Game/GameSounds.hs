module Macbeth.Wx.Game.GameSounds (
  gameSounds
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.Result
import Macbeth.Fics.FicsMessage
import Macbeth.OSX.Sounds
import qualified Macbeth.Wx.UserConfig as C


gameSounds :: C.Config -> Move -> FicsMessage -> IO ()
gameSounds config move msg = playSound config sound
  where sound = C.sounds config >>= findSound move msg


findSound :: Move -> FicsMessage -> C.Sounds -> Maybe String
findSound initMove msg config = case msg of
  GameMove Illegal _ -> C.illegal $ C.move $ C.game config
  GameMove Takeback{} _ -> C.takeback $ C.move $ C.game config
  GameMove _ move
    | not (C.enabledObservedGames config) && not (isGameUser move) -> Nothing
    | isCheck move -> C.check $ C.move $ C.game config
    | isCapture move -> C.capture $ C.move $ C.game config
    | isCastling move -> C.castling $ C.move $ C.game config
    | isDrop move -> C.pieceDrop $ C.move $ C.game config
    | otherwise -> C.normal $ C.move $ C.game config
  GameResult r
    | not (isGameUser initMove) && not (C.enabledObservedGames config) -> Nothing
    | isGameUser initMove && userWins initMove r -> C.youWin $ C.endOfGame $ C.game config
    | isGameUser initMove && userLoses initMove r -> C.youLose $ C.endOfGame $ C.game config
    | isGameUser initMove && (result r == Draw) -> C.youDraw $ C.endOfGame $ C.game config
    | result r == WhiteWins -> C.whiteWins $ C.endOfGame $ C.game config
    | result r == BlackWins -> C.blackWins $ C.endOfGame $ C.game config
    | result r == Draw -> C.draw $ C.endOfGame $ C.game config
    | result r == Aborted -> C.abort $ C.endOfGame $ C.game config
    | otherwise -> Nothing
  _ -> Nothing


userWins :: Move -> Result -> Bool
userWins m (Result _ _ _ _ r)
  | colorUser m == White && r == WhiteWins = True
  | colorUser m == Black && r == BlackWins = True
  | otherwise = False


userLoses :: Move -> Result -> Bool
userLoses m (Result _ _ _ _ r)
  | colorUser m == White && r == BlackWins = True
  | colorUser m == Black && r == WhiteWins = True
  | otherwise = False



{-

moveSound :: MoveModifier -> Move -> C.GameS -> Maybe String
moveSound _ move

--  | isCheckmake move = C.checkmate . C.endOfGame

  GameResult _ _ Draw -> C.draw $ C.endOfGame $ C.game s
  GameResult _ _ Aborted -> C.draw $ C.endOfGame $ C.game s
  GameResult {} -> Nothing -- is covered in moveSound
-}
