{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.Game.GameSounds (
  gameSounds
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.Result
import Macbeth.Fics.FicsMessage
import Macbeth.Wx.RuntimeEnv
import qualified Macbeth.Wx.Config.UserConfig as C

import Control.Applicative

gameSounds :: RuntimeEnv -> Move -> FicsMessage -> IO ()
gameSounds env move msg
  | not (isGameUser move) && not (env `getSoundConfig` C.enabledObservedGames) = return ()
  | otherwise = playSound env $ findSound move msg


findSound :: Move -> FicsMessage -> (C.Sounds -> Maybe String)
findSound initMove = \case
  GameMove Illegal _ -> C.illegal . C.move . C.game

  GameMove Takeback{} _ -> C.takeback . C.move . C.game

  GameMove _ move -> (\c ->
    isCheck move `withSound` C.check (C.move $ C.game c) <|>
    isCapture move `withSound` C.capture (C.move $ C.game c) <|>
    isCastling move `withSound` C.castling (C.move $ C.game c) <|>
    isDrop move `withSound` C.pieceDrop (C.move $ C.game c) <|>
    C.normal (C.move $ C.game c))

  GameResult r
    | isGameUser initMove && userWins initMove r -> C.youWin . C.endOfGame . C.game
    | isGameUser initMove && userLoses initMove r -> C.youLose . C.endOfGame . C.game
    | isGameUser initMove && (result r == Draw) -> C.youDraw . C.endOfGame . C.game
    | result r == WhiteWins -> C.whiteWins . C.endOfGame . C.game
    | result r == BlackWins -> C.blackWins . C.endOfGame . C.game
    | result r == Draw -> C.draw . C.endOfGame . C.game
    | result r == Aborted -> C.abort . C.endOfGame . C.game
    | otherwise -> const Nothing

  DrawRequest -> C.drawReq . C.request

  AbortRequest {} -> C.abortReq . C.request

  TakebackRequest {} -> C.takebackReq . C.request

  _ -> const Nothing

  where
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


