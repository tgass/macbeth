module Paths where

import           Data.List
import qualified Paths_macbeth_lib  as PM
import           System.Directory
import           System.Environment.FindBin
import           System.FilePath


getDataFileName :: FilePath -> IO FilePath
getDataFileName f
  | "app" `isInfixOf` __Bin__ = ((</> "Resources" </> f) . joinPath . init . splitPath) <$> getProgPath
  | otherwise = PM.getDataFileName f


getPiecesDir :: IO FilePath
getPiecesDir
  | "app" `isInfixOf` __Bin__ = ((</> "Resources" </> "pieces") . joinPath . init . splitPath) <$> getProgPath
  | otherwise = do
      dataDir <- PM.getDataDir
      return $ dataDir </> "pieces"


getMacbethUserDataDir :: FilePath -> IO FilePath
getMacbethUserDataDir file = do
  dir <- getAppUserDataDirectory "macbeth"
  return $ dir </> file

