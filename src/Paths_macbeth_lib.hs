module Paths_macbeth_lib where

import System.FilePath


getDataFileName :: FilePath -> IO FilePath
getDataFileName f = return $ "foo" </> f


getMacbethUserDataDir :: FilePath -> IO FilePath
getMacbethUserDataDir f = return $ ".temp" </> f
