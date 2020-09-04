module Main where

import           Data.List
import           Graphics.UI.WX
import           Macbeth.Fics.Connection
import           Macbeth.Fics.Timeseal
import           Macbeth.Wx.ToolBox
import           Macbeth.Wx.RuntimeEnv
import           Sound.ALUT
import           System.Directory
import           System.Environment.FindBin
import           System.FilePath


main :: IO ()
main = withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
  (h, chan) <- timesealConnection $ TimesealProvider zsealExec
  env <- initRuntime h
  start $ wxToolBox env chan


data TimesealProvider = TimesealProvider (IO FilePath)

instance TimesealEnv TimesealProvider where
  getTimesealExec (TimesealProvider x) = x


zsealExec :: IO FilePath
zsealExec = ((</> "Resources" </> "resources" </> "zseal") . joinPath . init . splitPath) <$> getProgPath
