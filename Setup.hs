import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       }

guiApps :: [MacApp]
guiApps = [MacApp "XChessRun"
                  "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/System/Library/Frameworks"
                  (Just "WxHello.icns")
                  Nothing -- Build a default Info.plist for the icon.
                  ["resources/bullhorn.gif"
                  ,"resources/dot-circle-o.gif"] -- No other resources.
                  [] -- No other binaries.
                  ChaseWithDefaults -- Try changing to ChaseWithDefaults
          ]

