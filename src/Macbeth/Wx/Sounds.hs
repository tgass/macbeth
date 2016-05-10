module Macbeth.Wx.Sounds (
  playSound
) where

import Macbeth.OSX.Afplay
import qualified Macbeth.Wx.Config.UserConfig as C
import Paths

import Control.Applicative
import Control.Monad.Trans.Maybe
import System.Directory
import System.FilePath


playSound :: C.Config -> Maybe String -> IO ()
playSound config sound = do
  path <- runMaybeT $ isSoundEnabled config *> findPath config sound
  mapM_ afplay path


isSoundEnabled :: C.Config -> MaybeT IO ()
isSoundEnabled config = MaybeT $ return $ mBoolToMaybe $ C.sounds config >>= Just . C.enabled
  where
    mBoolToMaybe :: Maybe Bool -> Maybe ()
    mBoolToMaybe (Just True) = Just ()
    mBoolToMaybe _ = Nothing


findPath :: C.Config -> Maybe String -> MaybeT IO FilePath
findPath config msg = do
  soundFile <- MaybeT $ return msg
  appSound <- MaybeT $ Just <$> getDataFileName ("sounds" </> soundFile)
  existsPath (C.directory config </> soundFile) <|> existsPath appSound


existsPath :: FilePath -> MaybeT IO FilePath
existsPath file = (MaybeT $ fmap boolToMaybe (doesFileExist file)) >>= \_ -> return file
  where
    boolToMaybe :: Bool -> Maybe ()
    boolToMaybe x = if x then Just () else Nothing




