{-# LANGUAGE LambdaCase #-}

module Macbeth.OSX.Sounds (
  sounds
) where

import Macbeth.Fics.FicsMessage
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


sounds :: Chan FicsMessage -> IO ()
sounds chan = readChan chan >>= \msg -> do
  config <- C.loadConfig
  path <- runMaybeT $ findPath config msg
  mapM_ playSound path


findPath :: C.Config -> FicsMessage -> MaybeT IO FilePath
findPath config msg = do
  soundFile <- MaybeT $ return (C.sounds config >>= mapSound msg)
  appSound <- MaybeT $ Just <$> getDataFileName ("sounds" </> soundFile)
  existsPath (C.directory config </> soundFile) <|> existsPath appSound


existsPath :: FilePath -> MaybeT IO FilePath
existsPath file = do
  _ <- MaybeT $ fmap (\x -> if x then Just x else Nothing) (doesFileExist file)
  return file


playSound :: FilePath -> IO ()
playSound = void . runCommand . ("afplay " ++)


mapSound :: FicsMessage -> C.Sounds -> Maybe String
mapSound msg s = case msg of
  LoggedIn {} -> Nothing
  MatchAccepted {} -> C.newGame $ C.game s
  GameMove mod move
    | not (C.enableObservedGames s) && not (isGameUser move) -> Nothing
    | otherwise -> moveSound mod move (C.move $ C.game s)
  _ -> Nothing


moveSound :: MoveModifier -> Move -> C.MoveS -> Maybe String
moveSound Illegal _ = C.illegal
moveSound _ move
  | isCheck move = C.check
  | otherwise = C.normal

