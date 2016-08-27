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
  , _username :: TVar String
}

initRuntime :: Handle -> IO RuntimeEnv
initRuntime h = do
  config <- C.initConfig
  RuntimeEnv h <$> return config <*> initSources <*> initBufferMap config <*> newTVarIO ""


username :: RuntimeEnv -> IO String
username = readTVarIO . _username


setUsername :: RuntimeEnv -> String -> IO ()
setUsername env username = atomically $ writeTVar (_username env) username


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
  source <- first sx
  case source of
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
initBufferMap config = do
  dir <- getDataFileName "sounds"
  appSounds <- loadSounds dir
  userSounds <- loadSounds (C.directory config)
  return (userSounds `M.union` appSounds)


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

