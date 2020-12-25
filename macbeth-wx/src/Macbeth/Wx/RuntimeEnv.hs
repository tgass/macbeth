{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Macbeth.Wx.RuntimeEnv (
  RuntimeEnv(handle, rtBoardConfig, rtSeekConfig),
  Tracking(..), untrackChat, trackChat, isTrackedChat, trackOngoingGame, trackObservingGame, untrackGame, isPlaying,
  reConfig, reIsAutoLogin, tOngoingGame, tObservingGames,
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
  pieceToBitmap,
  getLoginUsername,
  getLoginPassword
) where


import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Development.GitRev
import           Graphics.UI.WX hiding (play, get, when, size)
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Player hiding (handle)
import           Macbeth.Fics.Commands (HasHandle(..))
import           Macbeth.Fics.Commands.Seek (SeekConfig)
import           Macbeth.Utils.Utils
import           Macbeth.Wx.Config.UserConfig (Config (..), uUsername, uPassword, cUser, cAutologin)
import qualified Macbeth.Wx.Config.UserConfig as UserConfig
import           Macbeth.Wx.Config.BoardConfig (BoardConfig)
import qualified Macbeth.Wx.Config.BoardConfig as BC
import           Macbeth.Wx.Config.Sounds (Sounds)
import qualified Macbeth.Wx.Config.Sounds as Sounds
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
import           System.Log.Handler.Syslog


data RuntimeEnv = RuntimeEnv {
    handle :: Handle
  , nxtCommandId :: TVar Int
  , _reConfig :: Config
  , sources :: [Source]
  , pieceBitmapMap :: !(Map (PieceSet, Piece, Int) (Bitmap ()))
  , bufferMap :: !(Map String Buffer)
  , userHandle :: TVar UserHandle
  , rtBoardConfig :: TVar BoardConfig
  , rtSeekConfig :: TVar SeekConfig
  , _reIsAutoLogin :: TVar Bool
  , _reTracking :: TVar Tracking
}

instance HasHandle RuntimeEnv where
  getHandle = handle
  getCommandId env = do
    nxt <- readTVarIO $ nxtCommandId env
    atomically $ modifyTVar (nxtCommandId env) $ \i -> 
      if i == 10 then 1 else succ i
    return nxt


data Tracking = Tracking {
    _tOngoingGame :: Maybe GameId
  , _tObservingGames :: Set GameId
  , _tChats :: Set ChatId
}

makeLenses ''RuntimeEnv
makeLenses ''Tracking

initRuntime :: Handle -> IO RuntimeEnv
initRuntime h = do
  initLogger
  config <- UserConfig.initConfig
  RuntimeEnv 
    <$> pure h 
    <*> newTVarIO 1
    <*> pure config
    <*> initSources 
    <*> initPieceBitmaps
    <*> initBufferMap config 
    <*> newTVarIO emptyUserHandle 
    <*> (BC.convert (fromMaybe BC.defaultBoardConfig $ UserConfig.boardConfig config) (UserConfig.directory config) >>= newTVarIO)
    <*> (newTVarIO $ UserConfig.getSeekConfig config)
    <*> newTVarIO (config ^. cAutologin)
    <*> newTVarIO (Tracking Nothing Set.empty Set.empty)


username :: RuntimeEnv -> IO Username
username = fmap name . readTVarIO . userHandle


setUsername :: RuntimeEnv -> UserHandle -> IO ()
setUsername env = atomically . writeTVar (userHandle env)


setBoardConfig :: RuntimeEnv -> BoardConfig -> IO ()
setBoardConfig env = atomically . writeTVar (rtBoardConfig env) 


setSeekConfig :: RuntimeEnv -> SeekConfig -> IO ()
setSeekConfig env = atomically . writeTVar (rtSeekConfig env)


getConfig :: RuntimeEnv -> (Config -> a) -> a
getConfig env f = f $ env ^. reConfig


getVersion :: String
getVersion = take 7 $ $(gitHash)


pieceToBitmap :: RuntimeEnv -> PieceSet -> Piece -> Int -> Bitmap ()
pieceToBitmap env pieceSet piece size = pieceBitmapMap env Map.! (pieceSet, piece, size)


getSoundConfig :: RuntimeEnv -> (Sounds -> a) -> a
getSoundConfig env f = f $ UserConfig.soundsOrDef $ env ^. reConfig


-- Please remember, this is why I use ALUT. Switching to wx controlled sounds didn't work
-- https://forums.wxwidgets.org/viewtopic.php?t=43049
playSound :: RuntimeEnv -> (Sounds -> Maybe String) -> IO ()
playSound env f
  | Sounds.enabled soundConfig = play' (sources env) mBuffer
  | otherwise = return ()
  where soundConfig = UserConfig.soundsOrDef $ env ^. reConfig
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
  dir <- getSoundsDir
  appSounds <- loadSounds dir
  userSounds <- loadSounds $ UserConfig.directory c
  return $ userSounds `Map.union` appSounds


loadSounds :: FilePath -> IO (Map String Buffer)
loadSounds dir = do
  files <- filter ((== ".wav") . takeExtension) <$> getDirectoryContents dir
  fmap Map.fromList $ sequence $ flip fmap files $ \file -> do 
    buf <- createBuffer (File $ dir </> file)
    return (file, buf)


#ifdef CONSOLE_LOG
initLogger :: IO ()
initLogger = updateGlobalLogger rootLoggerName $ setLevel INFO
#else
initLogger :: IO ()
initLogger = do
  updateGlobalLogger rootLoggerName removeHandler
  syslogH <- openlog "macbeth" [PID] USER ERROR >>= \lh -> return $ 
      setFormatter lh (simpleLogFormatter "$time $msg")
  updateGlobalLogger rootLoggerName $ addHandler syslogH
#endif


getLoginUsername :: RuntimeEnv -> String
getLoginUsername env = fromMaybe "guest" $ env ^. reConfig ^? cUser . _Just . uUsername


getLoginPassword :: RuntimeEnv -> String
getLoginPassword env = 
  let mPassword = decrypt <$> env ^. reConfig ^? cUser . _Just . uPassword
  in case mPassword of
    Just pw   
      -- check that in case password was corrupted, there is at least one printable character send to fics server
      | any isPrint pw -> pw
      | otherwise -> ""
    Nothing -> ""


untrackGame :: RuntimeEnv -> GameId -> IO ()
untrackGame env gameId = atomically $ do
  tracking <- readTVar $ env ^. reTracking
  case tracking ^. tOngoingGame of
    Just gameId' -> when (gameId' == gameId) $ writeTVar (env ^. reTracking) $ tracking & tOngoingGame .~ Nothing
    Nothing -> return ()
  modifyTVar (env ^. reTracking) $ \t -> t & tObservingGames %~ Set.delete gameId


trackChat :: RuntimeEnv -> ChatId -> IO ()
trackChat env chatId = atomically $ modifyTVar (env ^. reTracking) $ \t -> t & tChats %~ Set.insert chatId


untrackChat :: RuntimeEnv -> ChatId -> IO ()
untrackChat env chatId = atomically $ modifyTVar (env ^. reTracking) $ \t -> t & tChats %~ Set.delete chatId


isTrackedChat :: RuntimeEnv -> ChatId -> IO Bool
isTrackedChat env chatId = do
  tracking <- readTVarIO (env ^. reTracking)
  return $ Set.member chatId $ tracking ^. tChats


trackOngoingGame :: RuntimeEnv -> GameId -> IO ()
trackOngoingGame env gameId = atomically $ modifyTVar (env ^. reTracking) $ \t -> t & tOngoingGame .~ Just gameId


trackObservingGame :: RuntimeEnv -> GameId -> IO ()
trackObservingGame env gameId = atomically $ modifyTVar (env ^. reTracking) $ \t -> t & tObservingGames %~ Set.insert gameId


isPlaying :: RuntimeEnv -> GameId -> IO Bool
isPlaying env gameId = do
  tracking <- readTVarIO $ env ^. reTracking
  return $ maybe False ((==) gameId) $ tracking ^. tOngoingGame

