module Paths where

import qualified Paths_macbeth_wx as PM
import           System.Directory
import           System.FilePath
import           System.IO.Unsafe

getMacbethUserDataDir :: IO FilePath
getMacbethUserDataDir = getAppUserDataDirectory "macbeth"


getMacbethUserConfigFile :: IO FilePath
getMacbethUserConfigFile = do
  dir <- getMacbethUserDataDir 
  return $ dir </> "macbeth.yaml"


getTileFilePath :: FilePath -> FilePath -> IO FilePath
getTileFilePath userDir filename = do
  let relTilePath = "tiles" </> filename
      fullPath = userDir </> relTilePath
  exists <- doesFileExist fullPath
  if exists
    then return fullPath
    else getDataFileName relTilePath


getDataFileName :: FilePath -> IO FilePath
getDataFileName path = PM.getDataFileName path


getPiecesDir :: IO FilePath
getPiecesDir = do
  dataDir <- PM.getDataDir
  return $ dataDir </> "pieces"


getSoundsDir :: IO FilePath
getSoundsDir = do
  dataDir <- PM.getDataDir
  return $ dataDir </> "sounds"


getIconPath :: String -> FilePath
getIconPath file = unsafePerformIO $ getDataFileName $ "icons" </> file
