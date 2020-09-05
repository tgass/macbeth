import Distribution.MacOSX
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Directory
import System.FilePath.Posix

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { postBuild = appBundleBuildHook guiApps }

guiApps :: [MacApp]
guiApps =
  [ MacApp
    { appName = "Macbeth"
    , appIcon = Just "resources/Macbeth.icns"
    , appPlist = Nothing -- Build a default Info.plist for the icon.
    , resources = [] -- No other resources.
    , otherBins = ["resources/zseal"] -- No other binaries.
    , appDeps = ChaseWithDefaults
    }
  ]

