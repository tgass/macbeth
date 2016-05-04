module Macbeth.OSX.Sounds (
  playSound
) where

import qualified Macbeth.Wx.UserConfig as C
import Paths

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import System.Directory
import System.FilePath
import System.Process


playSound :: C.Config -> Maybe String -> IO ()
playSound config sound = do
  path <- runMaybeT $ isSoundEnabled config *> findPath config sound
  mapM_ afplay path


isSoundEnabled :: C.Config -> MaybeT IO ()
isSoundEnabled config = do
  isOn <- MaybeT $ return (C.sounds config >>= Just . C.enabled)
  when isOn $ return ()


findPath :: C.Config -> Maybe String -> MaybeT IO FilePath
findPath config msg = do
  soundFile <- MaybeT $ return msg
  appSound <- MaybeT $ Just <$> getDataFileName ("sounds" </> soundFile)
  existsPath (C.directory config </> soundFile) <|> existsPath appSound


existsPath :: FilePath -> MaybeT IO FilePath
existsPath file = (MaybeT $ fmap boolToMaybe (doesFileExist file)) >>= \_ -> return file

boolToMaybe :: Bool -> Maybe ()
boolToMaybe x = if x then Just () else Nothing


afplay :: FilePath -> IO ()
afplay = void . runCommand . ("afplay " ++)
