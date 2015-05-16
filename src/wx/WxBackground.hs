module WxBackground (
  wxBackground
) where

import Move (playerColor)
import CommandMsg
import WxChallenge (wxChallenge)
import WxObservedGame (createObservedGame)
import WxUtils (eventLoop)

import Control.Concurrent (newEmptyMVar, takeMVar, forkIO, killThread)
import Control.Concurrent.Chan
import System.IO (Handle)
import Graphics.UI.WX
import Graphics.UI.WXCore

eventId = wxID_HIGHEST + 71

wxBackground :: Handle -> String -> Chan CommandMsg -> IO ()
wxBackground h name chan = do
  vCmd <- newEmptyMVar

  f <- frame [visible := False]

  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd -> do
    printCmdMsg cmd
    case cmd of
      AcceptChallenge move -> dupChan chan >>= createObservedGame h move (playerColor name move)

      c@(Challenge {}) -> wxChallenge h c

      _ -> return ()


  threadId <- forkIO $ eventLoop eventId chan vCmd f
  windowOnDestroy f $ killThread threadId

printCmdMsg :: CommandMsg -> IO ()
printCmdMsg (Sought _) = return ()
printCmdMsg Prompt = return ()
printCmdMsg cmd = print cmd
