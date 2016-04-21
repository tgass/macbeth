module Paths_Macbeth (
    getDataFileName
  , getAppDir
) where

import System.FilePath


getDataFileName :: FilePath -> IO FilePath
getDataFileName f = return $ "resources" </> f


getAppDir :: FilePath -> IO FilePath
getAppDir f = return $ "resources" </> f
