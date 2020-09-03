module Paths where

import           Data.List
import qualified Paths_macbeth_lib  as PM
import           System.Directory
import           System.Environment.FindBin
import           System.FilePath


getMacbethUserDataDir :: FilePath -> IO FilePath
getMacbethUserDataDir file = do
  dir <- getAppUserDataDirectory "macbeth"
  return $ dir </> file


getDataFileName :: FilePath -> IO FilePath
getDataFileName path
  | "app" `isInfixOf` __Bin__ = resources path
  | otherwise = PM.getDataFileName path


getPiecesDir :: IO FilePath
getPiecesDir
  | "app" `isInfixOf` __Bin__ = resources "pieces" 
  | otherwise = do
      dataDir <- PM.getDataDir
      return $ dataDir </> "pieces"


zsealExec :: IO FilePath
zsealExec
  | "app" `isInfixOf` __Bin__ = resources "zseal"
  | otherwise = do
      dir <- PM.getLibexecDir
      return $ dir </> "zseal"


resources :: FilePath -> IO FilePath
resources path = ((</> "Resources" </> path) . joinPath . init . splitPath) <$> getProgPath
