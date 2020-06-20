{-# LANGUAGE RecordWildCards #-}

module Macbeth.Wx.RuntimeEnv (
  RuntimeEnv(handle, rtBoardConfig, rtSeekConfig),
  username,
  setUsername,
  setBoardConfig,
  setSeekConfig,
  playSound,
  initRuntime,
  getConfig,
  getVersion,
  getSoundConfig,
  getIconFilePath,
  pieceToBitmap
) where


import           Control.Concurrent.STM
import           Control.Monad
import           Data.Maybe (fromMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Graphics.UI.WX hiding (play, get, when)
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Player
import           Macbeth.Fics.AppConfig
import           Macbeth.Wx.Config.UserConfig (Config (..))
import qualified Macbeth.Wx.Config.UserConfig as UserConfig
import           Macbeth.Wx.Config.BoardConfig (BoardConfig)
import qualified Macbeth.Wx.Config.BoardConfig as BC
import           Macbeth.Wx.Config.SeekConfig (SeekConfig)
import           Macbeth.Wx.Game.PieceSet
import           Paths
import           Sound.ALUT
import           System.FilePath
import           System.Directory
import           System.IO
import           System.IO.Unsafe
import           System.Log.Logger
import           System.Log.Handler.Simple hiding (priority)
import           System.Log.Handler (setFormatter)
import           System.Log.Formatter

data RuntimeEnv = RuntimeEnv {
    handle :: Handle
  , config :: Config
  , appConfig :: AppConfig
  , sources :: [Source]
  , pieceBitmapMap :: Map (PieceSet, Piece, Int) (Bitmap ())
  , bufferMap :: Map String Buffer
  , userHandle :: TVar UserHandle
  , rtBoardConfig :: TVar BoardConfig
  , rtSeekConfig :: TVar SeekConfig
}

initRuntime :: Handle -> IO RuntimeEnv
initRuntime h = do
  config <- UserConfig.initConfig
  appConfig' <- loadAppConfig
  initLogger $ stage appConfig'
  RuntimeEnv 
    <$> pure h 
    <*> pure config
    <*> pure appConfig' 
    <*> initSources 
    <*> initPieceBitmaps
    <*> initBufferMap config 
    <*> newTVarIO emptyUserHandle 
    <*> (BC.convert (fromMaybe BC.defaultBoardConfig $ UserConfig.boardConfig config) (UserConfig.directory config) >>= newTVarIO)
    <*> (newTVarIO $ UserConfig.getSeekConfig config)


username :: RuntimeEnv -> IO Username
username = fmap name . readTVarIO . userHandle


setUsername :: RuntimeEnv -> UserHandle -> IO ()
setUsername env = atomically . writeTVar (userHandle env)


setBoardConfig :: RuntimeEnv -> BoardConfig -> IO ()
setBoardConfig env = atomically . writeTVar (rtBoardConfig env) 


setSeekConfig :: RuntimeEnv -> SeekConfig -> IO ()
setSeekConfig env = atomically . writeTVar (rtSeekConfig env)


getConfig :: RuntimeEnv -> (Config -> a) -> a
getConfig env f = f $ config env


getVersion :: RuntimeEnv -> String
getVersion = version . appConfig


pieceToBitmap :: RuntimeEnv -> PieceSet -> Piece -> Int -> Bitmap ()
pieceToBitmap env pieceSet piece size = pieceBitmapMap env Map.! (pieceSet, piece, size)


getSoundConfig :: RuntimeEnv -> (UserConfig.Sounds -> a) -> a
getSoundConfig env f = f $ UserConfig.soundsOrDef $ config env


-- Please remember, this is why I use ALUT. Switching to wx controlled sounds didn't work
-- https://forums.wxwidgets.org/viewtopic.php?t=43049
playSound :: RuntimeEnv -> (UserConfig.Sounds -> Maybe String) -> IO ()
playSound env f
  | UserConfig.enabled soundConfig = play' (sources env) mBuffer
  | otherwise = return ()
  where soundConfig = UserConfig.soundsOrDef $ config env
        mBuffer = f soundConfig >>= (`Map.lookup` bufferMap env)


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


initBufferMap :: Config -> IO (Map String Buffer)
initBufferMap c = do
  dir <- getDataFileName "sounds"
  appSounds <- loadSounds dir
  userSounds <- loadSounds $ UserConfig.directory c
  return $ userSounds `Map.union` appSounds


loadSounds :: FilePath -> IO (Map String Buffer)
loadSounds dir = do
  files <- filter ((== ".wav") . takeExtension) <$> getDirectoryContents dir
  fmap Map.fromList $ sequence $ fmap mkPair files
  where
    mkPair :: FilePath -> IO (String, Buffer)
    mkPair f = do
      buffer <- createBuffer (File $ dir </> f)
      return (f, buffer)


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

