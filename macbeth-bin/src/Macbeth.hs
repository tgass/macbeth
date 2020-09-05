module Main where

import           Graphics.UI.WX
import           Macbeth.Fics.Connection
import           Macbeth.Fics.Timeseal
import           Macbeth.Wx.ToolBox
import           Macbeth.Wx.RuntimeEnv
import           Sound.ALUT
import qualified Paths_macbeth_bin as Paths
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
zsealExec = do
  dir <- Paths.getLibexecDir
  return $ dir </> "zseal"

