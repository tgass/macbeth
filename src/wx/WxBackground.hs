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
    putStrLn $ show cmd
    case cmd of
      Accept move -> dupChan chan >>= createObservedGame h move (playerColor name move)

      c@(Challenge _ _ _ _ _) -> wxChallenge h c

      _ -> return ()


  threadId <- forkIO $ eventLoop eventId chan vCmd f
  windowOnDestroy f $ killThread threadId
