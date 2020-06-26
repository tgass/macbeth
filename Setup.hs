import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { postBuild = appBundleBuildHook guiApps
  }

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

