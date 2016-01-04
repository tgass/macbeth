{-# LANGUAGE OverloadedStrings #-}

module Macbeth.Wx.ToolBox (
  wxToolBox
) where

import Macbeth.Fics.FicsMessage
--import Macbeth.Wx.About
import Macbeth.Wx.Configuration
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

eventId = wxID_HIGHEST + 51

wxToolBox :: Handle -> Chan FicsMessage -> IO ()
wxToolBox h chan = do
    f  <- frame [ text := "Macbeth"]

    tbar   <- toolBarEx f False False []
    tbarItem_seek <- toolItem tbar "Seek" False (unsafePerformIO $ getDataFileName $ "icons" </> "bullhorn.gif")
      [ on command := dupChan chan >>= wxSeek h False, enabled := False, tooltip := "Seek" ]

    tbarItem_match <- toolItem tbar "Match" False  (unsafePerformIO $ getDataFileName $ "icons" </> "dot-circle-o.gif")
      [ on command := dupChan chan >>= wxMatch h False, enabled := False, tooltip := "Match" ]

    tbarItem_finger <- toolItem tbar "Finger" False  (unsafePerformIO $ getDataFileName $ "icons" </> "fa-question.gif")
      [ on command := hPutStrLn h "4 finger", enabled := False, tooltip := "Finger"]

    status <- statusField []
    statusLoggedIn <- statusField [ statusWidth := 100]

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


    -- menu
    m_file   <- menuPane [text := "&File"]
    menuItem m_file [text := "Settings", on command := dupChan chan >>= wxConfiguration ]

    set f [ statusBar := [status, statusLoggedIn],
            layout := tabs nb
                        [ tab "Sought" $ container slp $ fill $ widget sl
                        , tab "Games" $ container glp $ fill $ widget gl
                        , tab "Pending" $ container pending $ fill $ widget pendingWidget
                        , tab "Console" $ container cp ( column 5  [ fill $ widget ct
                                                                   , hfill $ widget ce])
                        ]
          , menuBar := [m_file]
          , outerSize := sz 600 600
          ]

    -- preselect console
    notebookSetSelection nb 3

    vCmd <- newEmptyMVar
    threadId <- forkIO $ eventLoop eventId chan vCmd f
    windowOnDestroy f $ writeChan chan WxClose >> killThread threadId

    evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd ->
      glHandler cmd >> slHandler cmd >> pendingHandler cmd >> case cmd of

        NoSuchGame -> do
          set status [text := "No such game. Updating games..."]
          hPutStrLn h "4 games"

        SeekNotAvailable -> set status [text := "That seek is not available."]

        TextMessage text -> appendText ct $ text ++ "\n"

        SettingsDone -> hPutStrLn h `mapM_` ["4 iset seekinfo 1", "4 games", "4 pending"]

        InvalidPassword  -> void $ set status [text := "Invalid password."]

        LoggedIn userName -> set nb [on click := (onMouse nb >=> clickHandler h nb)] >>
                             hPutStrLn h `mapM_` [ "set seek 0", "set style 12", "iset nowrap 1", "iset block 1"] >>
                             set statusLoggedIn [ text := userName] >>
                             (`set` [ enabled := True ]) `mapM_` [tbarItem_seek, tbarItem_match, tbarItem_finger]

        GuestLogin _ -> set tbarItem_seek  [on command := dupChan chan >>= wxSeek h True ] >>
                        set tbarItem_match  [on command := dupChan chan >>= wxMatch h True ]

        Finger name stats -> dupChan chan >>= wxFinger name stats

        LoginTimeout -> set status [ text := "Login Timeout." ]

        Login -> dupChan chan >>= wxLogin h

        Observe move -> dupChan chan >>= createObservedGame h move

        MatchRequested c -> dupChan chan >>= wxChallenge h c

        MatchAccepted move -> do
          hPutStrLn h "4 pending" -- refresh pending list. Match might have been pending.
          dupChan chan >>= createObservedGame h move

        MatchDeclined user -> do
          hPutStrLn h "4 pending" -- refresh pending list. Match might have been pending.
          set status [text := user ++ " declines the match offer."]

        MatchUserNotLoggedIn user -> set status [text := user ++ " not logged in."]

        _ -> return ()


emitCommand :: TextCtrl () -> Handle -> IO ()
emitCommand textCtrl h = get textCtrl text >>= hPutStrLn h . ("5 " ++) >> set textCtrl [text := ""]


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
