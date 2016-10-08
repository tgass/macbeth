{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.Game.GameSounds (
  gameSounds
) where

import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.Result
import Macbeth.Fics.FicsMessage
import Macbeth.Wx.RuntimeEnv
import Macbeth.Wx.Game.BoardState
import qualified Macbeth.Wx.Config.UserConfig as C

import Control.Applicative

gameSounds :: RuntimeEnv -> BoardState -> FicsMessage -> IO ()
gameSounds env boardState msg
  | not (isGameUser boardState) && not (env `getSoundConfig` C.enabledObservedGames) = return ()
  | otherwise = playSound env $ findSound boardState msg


findSound :: BoardState -> FicsMessage -> (C.Sounds -> Maybe String)
findSound boardState = \case
  GameMove Illegal {} _ -> C.illegal . C.move . C.game

  GameMove Takeback {} _ -> C.takeback . C.move . C.game

  GameMove _ move' -> (\c ->
    isCheck move' `withSound` C.check (C.move $ C.game c) <|>
    isCapture move' `withSound` C.capture (C.move $ C.game c) <|>
    isCastling move' `withSound` C.castling (C.move $ C.game c) <|>
    isDrop move' `withSound` C.pieceDrop (C.move $ C.game c) <|>
    C.normal (C.move $ C.game c))

  GameResult (Result _ _ _ _ result')
    | isGameUser' && userWins isWhite' result' -> C.youWin . C.endOfGame . C.game
    | isGameUser' && not (userWins isWhite' result') -> C.youLose . C.endOfGame . C.game
    | isGameUser' && (result' == Draw) -> C.youDraw . C.endOfGame . C.game
    | result' == WhiteWins -> C.whiteWins . C.endOfGame . C.game
    | result' == BlackWins -> C.blackWins . C.endOfGame . C.game
    | result' == Draw -> C.draw . C.endOfGame . C.game
    | result' == Aborted -> C.abort . C.endOfGame . C.game
    | otherwise -> const Nothing

  DrawRequest {} -> C.drawReq . C.request

  AbortRequest {} -> C.abortReq . C.request

  TakebackRequest {} -> C.takebackReq . C.request

  _ -> const Nothing

  where
    withSound :: Bool -> Maybe String -> Maybe String
    withSound True (Just sound) = Just sound
    withSound _ _ = Nothing

    isWhite' = isWhiteUser boardState
    isGameUser' = isGameUser boardState


userWins :: Bool -> GameResult -> Bool
userWins isWhite result' =
  (isWhite && result' == WhiteWins) || (not isWhite && result' == BlackWins)

