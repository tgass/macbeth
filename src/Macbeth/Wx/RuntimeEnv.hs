module Macbeth.Wx.RuntimeEnv (
  RuntimeEnv(handle),
  username,
  setUsername,
  playSound,
  initRuntime,
  getConfig,
  getSoundConfig
) where

import qualified Macbeth.Wx.Config.UserConfig as C
import Macbeth.Fics.Api.Player

import Control.Concurrent.STM
import Paths
import Sound.ALUT
import System.FilePath
import System.Directory
import System.IO
import qualified Data.HashMap as M

data RuntimeEnv = RuntimeEnv {
    handle :: Handle
  , config :: C.Config
  , sources :: [Source]
  , bufferMap :: M.Map String Buffer
  , userHandle :: TVar UserHandle
}

initRuntime :: Handle -> IO RuntimeEnv
initRuntime h = do
  c <- C.initConfig
  RuntimeEnv h <$> return c <*> initSources <*> initBufferMap c <*> newTVarIO emptyUserHandle


username :: RuntimeEnv -> IO Username
username = fmap name . readTVarIO . userHandle


setUsername :: RuntimeEnv -> UserHandle -> IO ()
setUsername env = atomically . writeTVar (userHandle env)


getConfig :: RuntimeEnv -> (C.Config -> a) -> a
getConfig env f = f $ config env


getSoundConfig :: RuntimeEnv -> (C.Sounds -> a) -> a
getSoundConfig env f = f $ C.soundsOrDef $ config env


playSound :: RuntimeEnv -> (C.Sounds -> Maybe String) -> IO ()
playSound env f
  | C.enabled soundConfig = play' (sources env) mBuffer
  | otherwise = return ()
  where soundConfig = C.soundsOrDef $ config env
        mBuffer = f soundConfig >>= (`M.lookup` bufferMap env)


play' :: [Source] -> Maybe Buffer -> IO ()
play' sx b = do
  mSource <- first sx
  case mSource of
    Just source -> do
      buffer source $= b
      play [source]
    Nothing -> return ()


first :: [Source] -> IO (Maybe Source)
first [] = return Nothing
first (s:sx) = do
  state <- get (sourceState s)
  if state /= Playing then return (Just s) else first sx


initSources :: IO [Source]
initSources = do
  (Just device) <- openDevice Nothing
  (Just context) <- createContext device []
  currentContext $= Just context
  genObjectNames 20


initBufferMap :: C.Config -> IO (M.Map String Buffer)
initBufferMap c = do
  dir <- getDataFileName "sounds"
  appSounds <- loadSounds dir
  userSounds <- loadSounds $ C.directory c
  return $ userSounds `M.union` appSounds


loadSounds :: FilePath -> IO (M.Map String Buffer)
loadSounds dir = do
  files <- filter ((== ".wav") . takeExtension) <$> getDirectoryContents dir
  bufferTuples <- bar $ fmap (\f -> dir </> f) files
  return $ M.fromList bufferTuples
  where

    bar :: [FilePath] -> IO [(String, Buffer)]
    bar fx = sequence $ fmap foo fx

    foo :: FilePath -> IO (String, Buffer)
    foo f = (,) (takeFileName f) <$> createBuffer (File f)

