module Paths_Macbeth (
  getDataFileName
) where

import System.FilePath

getDataFileName :: FilePath -> IO FilePath
getDataFileName f = return $ "resources" </> f
