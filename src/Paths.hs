module Paths (
    getDataFileName
  , getAppDir
) where

import Data.List
import qualified Paths_Macbeth as PM
import System.Directory
import System.Environment.FindBin
import System.FilePath


getDataFileName :: FilePath -> IO FilePath
getDataFileName f
  | "app" `isInfixOf` __Bin__ = ((</> "Resources" </> f) . joinPath . init . splitPath) <$> getProgPath
  | otherwise = PM.getDataFileName f


getAppDir :: FilePath -> IO FilePath
getAppDir file
  | "app" `isInfixOf` __Bin__ = (</> file) <$> getAppUserDataDirectory "macbeth"
  | otherwise = PM.getAppDir file

