module Macbeth.Wx.Sounds (
  Sounds(..),
  initSound,
  playSound
) where

import qualified Macbeth.Wx.Config.UserConfig as C
import Paths

import System.FilePath
import System.Directory
import Sound.ALUT
import qualified Data.HashMap as M


data Sounds = Sounds [Source] (M.Map String Buffer)


initSound :: IO Sounds
initSound = do
  config <- C.initConfig
  (Just device) <- openDevice Nothing
  (Just context) <- createContext device []
  currentContext $= Just context
  sources <- genObjectNames 20
  dir <- getDataFileName "sounds"
  appSounds <- loadSounds dir
  userSounds <- loadSounds (C.directory config)
  return $ Sounds sources (userSounds `M.union` appSounds)


loadSounds :: FilePath -> IO (M.Map String Buffer)
loadSounds dir = do
  files <- filter ((== ".wav") . takeExtension) <$> getDirectoryContents dir
  bufferTuples <- bar $ fmap (\f -> dir </> f) files
  return $ M.fromList bufferTuples
  where

    bar :: [FilePath] -> IO [(String, Buffer)]
    bar fx = sequence $ fmap foo fx

    foo :: FilePath -> IO (String, Buffer)
    foo f = do
      buffer <- createBuffer $ File f
      return (takeFileName f, buffer)


playSound :: Maybe C.Sounds -> Maybe String -> Sounds -> IO ()
playSound Nothing _ _ = return ()
playSound _ Nothing _ = return ()
playSound (Just soundC) (Just sound) (Sounds sx bx)
  | C.enabled soundC = case sound `M.lookup` bx of
      Just b -> play' sx b
      _ -> return ()
  | otherwise = return ()


play' :: [Source] -> Buffer -> IO ()
play' sx b = do
  source <- first sx
  case source of
    Just source -> do
      buffer source $= Just b
      play [source]
    Nothing -> return ()


first :: [Source] -> IO (Maybe Source)
first [] = return Nothing
first (s:sx) = do
  state <- get (sourceState s)
  if state /= Playing then return (Just s) else first sx

