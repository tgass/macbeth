module Macbeth.OSX.Sounds (
  sounds
) where

import Macbeth.Fics.FicsMessage
import qualified Macbeth.Wx.UserConfig as C
import Paths

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import System.Directory
import System.FilePath
import System.Process


sounds :: C.Config -> (FicsMessage -> Maybe String) -> FicsMessage -> IO ()
sounds config f msg = do
  path <- runMaybeT $ findPath config (f msg)
  mapM_ afplay path


findPath :: C.Config -> Maybe String -> MaybeT IO FilePath
findPath config msg = do
  soundFile <- MaybeT $ return msg
  appSound <- MaybeT $ Just <$> getDataFileName ("sounds" </> soundFile)
  existsPath (C.directory config </> soundFile) <|> existsPath appSound


existsPath :: FilePath -> MaybeT IO FilePath
existsPath file = (MaybeT $ fmap boolToMaybe (doesFileExist file)) >>= \_ -> return file
  where boolToMaybe x = if x then Just () else Nothing


afplay :: FilePath -> IO ()
afplay = void . runCommand . ("afplay " ++)
