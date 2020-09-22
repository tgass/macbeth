module Macbeth.Wx.Game.GameSounds (
  gameSounds
) where

import           Control.Applicative
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Move
import           Macbeth.Fics.Api.Result
import           Macbeth.Fics.Message
import           Macbeth.Wx.RuntimeEnv
import           Macbeth.Wx.Game.BoardState
import qualified Macbeth.Wx.Config.Sounds as S


gameSounds :: RuntimeEnv -> BoardState -> Message -> IO ()
gameSounds env boardState msg
  | not (isGameUser boardState) && not (env `getSoundConfig` S.enabledObservedGames) = return ()
  | otherwise = playSound env $ findSound boardState msg


findSound :: BoardState -> Message -> (S.Sounds -> Maybe String)
findSound boardState = \case
  GameMove Illegal {} _ -> S.illegal . S.move . S.game

  GameMove Takeback {} _ -> S.takeback . S.move . S.game

  GameMove _ move -> (\c ->
         isCheck move `withSound` S.check (S.move $ S.game c) 
    <|> isCapture move `withSound` S.capture (S.move $ S.game c)
    <|> isCastling move `withSound` S.castling (S.move $ S.game c)
    <|> isDrop move `withSound` S.pieceDrop (S.move $ S.game c) 
    <|> S.normal (S.move $ S.game c))

  GameResult (Result _ _ _ _ result)
    | userWins (userColor_ boardState) result -> S.youWin . S.endOfGame . S.game
    | userLoses (userColor_ boardState) result -> S.youLose . S.endOfGame . S.game
    | userDraws (userColor_ boardState) result  -> S.youDraw . S.endOfGame . S.game
    | result == WhiteWins -> S.whiteWins . S.endOfGame . S.game
    | result == BlackWins -> S.blackWins . S.endOfGame . S.game
    | result == Draw -> S.draw . S.endOfGame . S.game
    | result == Aborted -> S.abort . S.endOfGame . S.game
    | otherwise -> const Nothing

  DrawRequest {} -> S.drawReq . S.request

  AbortRequest {} -> S.abortReq . S.request

  TakebackRequest {} -> S.takebackReq . S.request

  _ -> const Nothing

  where
    withSound :: Bool -> Maybe String -> Maybe String
    withSound True (Just sound) = Just sound
    withSound _ _ = Nothing


userWins :: Maybe PColor -> GameResult -> Bool
userWins Nothing _ = False
userWins (Just White) result = result == WhiteWins
userWins (Just Black) result = result == BlackWins


userLoses :: Maybe PColor -> GameResult -> Bool
userLoses Nothing _ = False
userLoses (Just Black) result = result == WhiteWins
userLoses (Just White) result = result == BlackWins


userDraws :: Maybe PColor -> GameResult -> Bool
userDraws Nothing _ = False
userDraws _ result = result == Draw

