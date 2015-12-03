module Paths (
  getDataFileName
) where

import Data.List
import qualified Paths_Macbeth as PM
import System.Environment.FindBin
import System.FilePath
import Control.Applicative

getDataFileName :: FilePath -> IO FilePath
getDataFileName f
  | "app" `isInfixOf` __Bin__ = ((</> "Resources" </> f) . joinPath . init . splitPath) <$> getProgPath
  | otherwise = PM.getDataFileName f
