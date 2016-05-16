module Macbeth.Wx.Game.GameSounds (
  gameSounds
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.Result
import Macbeth.Fics.FicsMessage
import Macbeth.Wx.Sounds
import qualified Macbeth.Wx.Config.UserConfig as C

import Control.Applicative

gameSounds :: Maybe C.Sounds -> Move -> FicsMessage -> Sounds -> IO ()
gameSounds Nothing _ _ _ = return ()
gameSounds (Just soundC) move msg sounds = playSound (Just soundC) soundFile sounds
  where soundFile =  findSound move msg soundC


findSound :: Move -> FicsMessage -> C.Sounds -> Maybe String
findSound initMove msg config =
  if not (isGameUser initMove) && not (C.enabledObservedGames config) then Nothing
  else case msg of
  GameMove Illegal _ -> C.illegal $ C.move $ C.game config

  GameMove Takeback{} _ -> C.takeback moveS

  GameMove _ move ->
    (isCheck move `withSound` C.check moveS) <|>
    (isCapture move `withSound` C.capture moveS) <|>
    (isCastling move `withSound` C.castling moveS) <|>
    (isDrop move `withSound` C.pieceDrop moveS) <|>
    C.normal moveS

  GameResult r
    | isGameUser initMove && userWins initMove r -> C.youWin endOfGameS
    | isGameUser initMove && userLoses initMove r -> C.youLose endOfGameS
    | isGameUser initMove && (result r == Draw) -> C.youDraw endOfGameS
    | result r == WhiteWins -> C.whiteWins endOfGameS
    | result r == BlackWins -> C.blackWins endOfGameS
    | result r == Draw -> C.draw endOfGameS
    | result r == Aborted -> C.abort endOfGameS
    | otherwise -> Nothing

  DrawRequest -> C.drawReq $ C.request config

  AbortRequest {} -> C.abortReq $ C.request config

  TakebackRequest {} -> C.takebackReq $ C.request config

  _ -> Nothing

  where
    endOfGameS = C.endOfGame $ C.game config
    moveS = C.move $ C.game config

    withSound :: Bool -> Maybe String -> Maybe String
    withSound True (Just sound) = Just sound
    withSound _ _ = Nothing


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


