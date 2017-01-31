module Macbeth.Wx.RuntimeEnv (
  RuntimeEnv(handle),
  username,
  setUsername,
  playSound,
  initRuntime,
  getConfig,
  getSoundConfig,
  getIconFilePath
) where

import qualified Macbeth.Wx.Config.UserConfig as C
import Macbeth.Fics.Api.Player
import Macbeth.Fics.AppConfig

import Control.Concurrent.STM
import Control.Monad
import Paths
import Sound.ALUT
import System.FilePath
import System.Directory
import System.IO
import System.IO.Unsafe
import qualified Data.HashMap.Strict as M
import System.Log.Logger
import System.Log.Handler.Simple hiding (priority)
import System.Log.Handler (setFormatter)
import System.Log.Formatter

data RuntimeEnv = RuntimeEnv {
    handle :: Handle
  , config :: C.Config
  , sources :: [Source]
  , bufferMap :: M.HashMap String Buffer
  , userHandle :: TVar UserHandle
}

initRuntime :: Handle -> IO RuntimeEnv
initRuntime h = do
  c <- C.initConfig
  initLogger . stage =<< loadAppConfig
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


getIconFilePath :: String -> FilePath
getIconFilePath = unsafePerformIO . getDataFileName . ("icons" </>) . (++ ".gif")


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


initBufferMap :: C.Config -> IO (M.HashMap String Buffer)
initBufferMap c = do
  dir <- getDataFileName "sounds"
  appSounds <- loadSounds dir
  userSounds <- loadSounds $ C.directory c
  return $ userSounds `M.union` appSounds


loadSounds :: FilePath -> IO (M.HashMap String Buffer)
loadSounds dir = do
  files <- filter ((== ".wav") . takeExtension) <$> getDirectoryContents dir
  bufferTuples <- bar $ fmap (\f -> dir </> f) files
  return $ M.fromList bufferTuples
  where

    bar :: [FilePath] -> IO [(String, Buffer)]
    bar fx = sequence $ fmap foo fx

    foo :: FilePath -> IO (String, Buffer)
    foo f = (,) (takeFileName f) <$> createBuffer (File f)


initLogger :: Stage -> IO ()
initLogger stage' = do
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName $ setLevel DEBUG

  fileH <- fileHandler "/tmp/macbeth.log" INFO >>= \lh -> return $
       setFormatter lh (simpleLogFormatter "$time $msg")

  stdOutH <- streamHandler stdout INFO >>= \lh -> return $
       setFormatter lh (simpleLogFormatter "$time $msg")

  updateGlobalLogger rootLoggerName (addHandler fileH)
  when (stage' == Dev) $ updateGlobalLogger "console" (addHandler stdOutH)

