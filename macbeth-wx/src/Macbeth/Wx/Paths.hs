module Macbeth.Wx.Paths where

import qualified Paths_macbeth_wx as AutoPath
import           System.Directory
import           System.FilePath
import           System.IO.Unsafe

macbethUserDataDir :: IO FilePath
macbethUserDataDir = getAppUserDataDirectory "macbeth"


macbethUserConfigFile :: IO FilePath
macbethUserConfigFile = do
  dir <- macbethUserDataDir 
  return $ dir </> "macbeth.yaml"


tileFilepath :: FilePath -> FilePath -> IO FilePath
tileFilepath userDir filename = do
  let relTilePath = "tiles" </> filename
      fullPath = userDir </> relTilePath
  exists <- doesFileExist fullPath
  if exists
    then return fullPath
    else AutoPath.getDataFileName relTilePath


piecesDir :: IO FilePath
piecesDir = do
  dataDir <- AutoPath.getDataDir
  return $ dataDir </> "pieces"


soundsDir :: IO FilePath
soundsDir = do
  dataDir <- AutoPath.getDataDir
  return $ dataDir </> "sounds"


iconFilepath :: String -> FilePath
iconFilepath file = unsafePerformIO $ AutoPath.getDataFileName $ "icons" </> file
