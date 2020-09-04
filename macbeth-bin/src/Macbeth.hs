module Main where

import           Data.List
import           Graphics.UI.WX
import           Macbeth.Fics.Connection
import           Macbeth.Fics.Timeseal
import           Macbeth.Wx.ToolBox
import           Macbeth.Wx.RuntimeEnv
import           Sound.ALUT
import qualified Paths_macbeth_bin as Paths
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
zsealExec
  | "app" `isInfixOf` __Bin__ = resources "zseal"
  | otherwise = do
      dir <- Paths.getLibexecDir
      return $ dir </> "zseal"


resources :: FilePath -> IO FilePath
resources path = ((</> "Resources" </> path) . joinPath . init . splitPath) <$> getProgPath
