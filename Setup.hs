import Distribution.MacOSX
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Directory
import System.FilePath.Posix

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
 { 
--    postBuild = appBundleBuildHook guiApps
      postCopy = myPostCopy
 }

myPostCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostCopy _ _ pdesc lbi = do
  putStrLn "macbeth-setup> Copying zseal..."
  let zsealBinary = "zseal"
      installDirs = absoluteInstallDirs pdesc lbi NoCopyDest :: InstallDirs FilePath
  createDirectoryIfMissing False $ libexecdir installDirs
  copyFile (dataDir pdesc </> zsealBinary)  $ libexecdir installDirs </> zsealBinary

guiApps :: [MacApp]
guiApps =
  [ MacApp
    { appName = "Macbeth"
    , appIcon = Just "resources/Macbeth.icns"
    , appPlist = Nothing -- Build a default Info.plist for the icon.
    , resources = [] -- No other resources.
    , otherBins = [] -- No other binaries.
    , appDeps = ChaseWithDefaults
    }
  ]

