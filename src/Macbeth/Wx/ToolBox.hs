{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Wx.ToolBox (
  wxToolBox
) where

import Macbeth.Api.CommandMsg
import Macbeth.Wx.About
import Macbeth.Wx.Finger
import Macbeth.Wx.GamesList
import Macbeth.Wx.Login
import Macbeth.Wx.Match
import Macbeth.Wx.Seek
import Macbeth.Wx.Utils
import Macbeth.Wx.SoughtList
import Macbeth.Wx.ObservedGame
import Macbeth.Wx.Challenge
import Macbeth.Wx.Pending
import Paths

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import System.FilePath
import System.IO
import System.IO.Unsafe

ficsEventId = wxID_HIGHEST + 51

wxToolBox :: Handle -> Chan CommandMsg -> IO ()
wxToolBox h chan = do
    f  <- frame [ text := "Macbeth"]

    tbar   <- toolBarEx f False False []
    tbarItem_seek <- toolItem tbar "Seek" False (unsafePerformIO $ getDataFileName $ "icons" </> "bullhorn.gif")
      [ on command := wxSeek h False, enabled := False, tooltip := "Seek" ]

    tbarItem_match <- toolItem tbar "Match" False  (unsafePerformIO $ getDataFileName $ "icons" </> "dot-circle-o.gif")
      [ on command := wxMatch h False, enabled := False, tooltip := "Match" ]

    tbarItem_finger <- toolItem tbar "Finger" False  (unsafePerformIO $ getDataFileName $ "icons" </> "fa-question.png")
      [ on command := hPutStrLn h "4 finger", enabled := False, tooltip := "Finger"]

    status <- statusField []

    nb <- notebook f []

    -- Sought list
    slp <- panel nb []
    (sl, slHandler) <- wxSoughtList slp h

    -- Games list
    glp <- panel nb []
    (gl, glHandler)  <- wxGamesList glp h

    -- Pending
    pending <- panel nb []
    (pendingWidget, pendingHandler) <- wxPending h pending

    -- Console
    cp <- panel nb []
    ct <- textCtrlEx cp (wxTE_MULTILINE .+. wxTE_RICH .+. wxTE_READONLY) [font := fontFixed]
    ce <- entry cp []
    set ce [on enterKey := emitCommand ce h]


    -- about menu
    m_help    <- menuHelp []
    _         <- menuAbout m_help [help := "About Macbeth", on command := wxAbout ]

    set f [ layout := tabs nb
                        [ tab "Sought" $ container slp $ fill $ widget sl
                        , tab "Games" $ container glp $ fill $ widget gl
                        , tab "Pending" $ container pending $ fill $ widget pendingWidget
                        , tab "Console" $ container cp ( column 5  [ fill $ widget ct
                                                                   , hfill $ widget ce])
                        ]
          , menuBar := [m_help]
          , outerSize := sz 600 600
          , statusBar := [status]
          ]

    -- preselect console
    notebookSetSelection nb 3

    vCmd <- newEmptyMVar
    threadId <- forkIO $ eventLoop ficsEventId chan vCmd f
    windowOnDestroy f $ killThread threadId
    evtHandlerOnMenuCommand f ficsEventId $ takeMVar vCmd >>= \cmd ->
      glHandler cmd >> slHandler cmd >> pendingHandler cmd >> case cmd of

        MatchUserNotLoggedIn user -> set status [text := user ++ " not logged in."]

        NoSuchGame -> do
          set status [text := "No such game. Updating games..."]
          hPutStrLn h "4 games"

        TextMessage text -> appendText ct $ text ++ "\n"

        SettingsDone -> hPutStrLn h `mapM_` ["4 iset seekinfo 1", "4 games", "4 pending"]

        InvalidPassword  -> void $ set status [text := "Invalid password."]

        LoggedIn userName -> set nb [on click := (onMouse nb >=> clickHandler h nb)] >>
                             hPutStrLn h `mapM_` defaultParams >>
                             set f [ text := "Macbeth - Logged in as " ++ userName ] >>
                             set tbarItem_seek [ enabled := True ] >>
                             set tbarItem_match [ enabled := True ] >>
                             set tbarItem_finger [ enabled := True ]

        GuestLogin _ -> set tbarItem_seek  [on command := wxSeek h True ] >>
                        set tbarItem_match  [on command := wxMatch h True ]

        Finger name stats -> wxFinger name stats

        LoginTimeout -> set f [ text := "Login Timeout." ]

        Login -> dupChan chan >>= wxLogin h

        Observe move -> dupChan chan >>= createObservedGame h move

        MatchAccepted move -> dupChan chan >>= createObservedGame h move

        MatchRequested c -> dupChan chan >>= wxChallenge h c

        _ -> return ()


emitCommand :: TextCtrl () -> Handle -> IO ()
emitCommand textCtrl h = get textCtrl text >>= hPutStrLn h . ("5 " ++) >> set textCtrl [text := ""]


defaultParams = [ "set seek 0", "set style 12", "iset nowrap 1", "iset block 1"]


onMouse :: Notebook() -> Point -> IO Int
onMouse nb p = propagateEvent >> notebookHitTest nb p flag

clickHandler :: Handle -> Notebook () -> Int -> IO ()
clickHandler h nb idx = notebookGetPageText nb idx >>= \text -> case text of
  "Pending" -> hPutStrLn h "5 pending"
  "Games" -> hPutStrLn h "5 games"
  _ -> return ()


{-# NOINLINE flag #-}
flag :: Ptr CInt
flag  =  unsafePerformIO flag'
  where flag' = do
             work <- malloc::IO (Ptr CInt)
             poke work (fromIntegral wxBK_HITTEST_ONPAGE)
             return work
