module Paths where

import qualified Paths_macbeth_wx as PM
import           System.Directory
import           System.FilePath


getMacbethUserDataDir :: FilePath -> IO FilePath
getMacbethUserDataDir file = do
  dir <- getAppUserDataDirectory "macbeth"
  return $ dir </> file


getDataFileName :: FilePath -> IO FilePath
getDataFileName path = PM.getDataFileName path


getPiecesDir :: IO FilePath
getPiecesDir = do
  dataDir <- PM.getDataDir
  return $ dataDir </> "pieces"

