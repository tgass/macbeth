import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Directory
import System.FilePath.Posix

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { postCopy = myPostCopy }

myPostCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostCopy _ _ pdesc lbi = do
  putStrLn "macbeth-setup> Copying zseal..."
  let zsealBinary = "zseal"
      installDirs = absoluteInstallDirs pdesc lbi NoCopyDest :: InstallDirs FilePath
  createDirectoryIfMissing False $ libexecdir installDirs
  copyFile (dataDir pdesc </> zsealBinary)  $ libexecdir installDirs </> zsealBinary
