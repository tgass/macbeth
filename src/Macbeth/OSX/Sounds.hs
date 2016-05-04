{-# LANGUAGE LambdaCase #-}

module Macbeth.OSX.Sounds (
  sounds
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Game
import Macbeth.Fics.Api.Move
import qualified Macbeth.Wx.UserConfig as C
import Paths

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Maybe
import System.Directory
import System.FilePath
import System.Process


sounds :: C.Config -> FicsMessage -> IO ()
sounds config msg = do
  path <- runMaybeT $ findPath config msg
  mapM_ playSound path


findPath :: C.Config -> FicsMessage -> MaybeT IO FilePath
findPath config msg = do
  soundFile <- MaybeT $ return (C.sounds config >>= mapSound msg)
  appSound <- MaybeT $ Just <$> getDataFileName ("sounds" </> soundFile)
  existsPath (C.directory config </> soundFile) <|> existsPath appSound


existsPath :: FilePath -> MaybeT IO FilePath
existsPath file = (MaybeT $ fmap boolToMaybe (doesFileExist file)) >>= \_ -> return file
  where boolToMaybe x = if x then Just () else Nothing


playSound :: FilePath -> IO ()
playSound = void . runCommand . ("afplay " ++)


mapSound :: FicsMessage -> C.Sounds -> Maybe String
mapSound msg s = case msg of
  LoggedIn {} -> C.logonToServer $ C.other s
  MatchAccepted {} -> C.newGame $ C.game s
  GameResult _ _ Draw -> C.draw $ C.endOfGame $ C.game s
  GameResult _ _ Aborted -> C.draw $ C.endOfGame $ C.game s
  GameResult {} -> Nothing -- is covered in moveSound
  GameMove mod move
    | not (C.enableObservedGames s) && not (isGameUser move) -> Nothing
    | otherwise -> moveSound mod move (C.game s)
  _ -> Nothing


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

