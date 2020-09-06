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
import qualified Macbeth.Wx.Config.UserConfig as C


gameSounds :: RuntimeEnv -> BoardState -> Message -> IO ()
gameSounds env boardState msg
  | not (isGameUser boardState) && not (env `getSoundConfig` C.enabledObservedGames) = return ()
  | otherwise = playSound env $ findSound boardState msg


findSound :: BoardState -> Message -> (C.Sounds -> Maybe String)
findSound boardState = \case
  GameMove Illegal {} _ -> C.illegal . C.move . C.game

  GameMove Takeback {} _ -> C.takeback . C.move . C.game

  GameMove _ move -> (\c ->
         isCheck move `withSound` C.check (C.move $ C.game c) 
    <|> isCapture move `withSound` C.capture (C.move $ C.game c)
    <|> isCastling move `withSound` C.castling (C.move $ C.game c)
    <|> isDrop move `withSound` C.pieceDrop (C.move $ C.game c) 
    <|> C.normal (C.move $ C.game c))

  GameResult (Result _ _ _ _ result)
    | userWins (userColor_ boardState) result -> C.youWin . C.endOfGame . C.game
    | userLoses (userColor_ boardState) result -> C.youLose . C.endOfGame . C.game
    | userDraws (userColor_ boardState) result  -> C.youDraw . C.endOfGame . C.game
    | result == WhiteWins -> C.whiteWins . C.endOfGame . C.game
    | result == BlackWins -> C.blackWins . C.endOfGame . C.game
    | result == Draw -> C.draw . C.endOfGame . C.game
    | result == Aborted -> C.abort . C.endOfGame . C.game
    | otherwise -> const Nothing

  DrawRequest {} -> C.drawReq . C.request

  AbortRequest {} -> C.abortReq . C.request

  TakebackRequest {} -> C.takebackReq . C.request

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

