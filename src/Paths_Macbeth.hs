module Paths_Macbeth (
  getDataDir,
  getDataFileName
) where

root = "resources/"

getDataDir :: IO FilePath
getDataDir = return root

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return $ root ++ name
