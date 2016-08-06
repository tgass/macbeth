module Paths_Macbeth (
    getDataFileName
  , getMacbethUserDataDir
) where

import System.FilePath


getDataFileName :: FilePath -> IO FilePath
getDataFileName f = return $ "resources" </> f


getMacbethUserDataDir :: FilePath -> IO FilePath
getMacbethUserDataDir f = return $ ".temp" </> f
