module Macbeth.Fics.Timeseal where

class TimesealEnv a where
  getTimesealExec :: a -> IO FilePath
