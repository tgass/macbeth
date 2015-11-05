{-# LANGUAGE OverloadedStrings #-}

module Lentils.Wx.ToolBox (
  wxToolBox
) where

import Lentils.Api.CommandMsg
import Lentils.Api.Move
import Lentils.Wx.About
import Lentils.Wx.Finger
import Lentils.Wx.GamesList
import Lentils.Wx.Login
import Lentils.Wx.Match
import Lentils.Wx.Seek
import Lentils.Wx.Utils
import Lentils.Wx.SoughtList
import Lentils.Wx.ObservedGame
import Lentils.Wx.Challenge
import Lentils.Wx.Pending

import Paths_XChess

import Control.Concurrent
import Control.Concurrent.Chan ()
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO
import qualified Control.Monad as M (when, void)

ficsEventId = wxID_HIGHEST + 51

wxToolBox :: Handle -> Chan CommandMsg -> IO ()
wxToolBox h chan = do
    dataDir <- getDataDir

    -- main frame
    f  <- frame [ text := "MacBeth" ]


    tbar   <- toolBar f []
    tbarItem_seek <- toolItem tbar "Seek" False (dataDir ++ "bullhorn.gif")
      [ on command := wxSeek h False, enabled := False ]

    tbarItem_match <- toolItem tbar "Match" False  (dataDir ++ "dot-circle-o.gif")
      [ on command := wxMatch h False, enabled := False]

    tbarItem_finger <- toolItem tbar "Finger" False  (dataDir ++ "fa-question.png")
      [ on command := hPutStrLn h "4 finger", enabled := False]

    status <- statusField []


    nb <- notebook f []
    notebookPageChanged nb (\evt -> notebookGetSelection nb >>= print)

    -- Sought list
    slp <- panel nb []
    (sl, slHandler) <- wxSoughtList slp h

    -- Pending
    pending <- panel nb []
    (pendingWidget, pendingHandler) <- wxPending pending

    -- Console
    cp <- panel nb []
    ct <- textCtrlEx cp (wxTE_MULTILINE .+. wxTE_RICH .+. wxTE_READONLY) [font := fontFixed]
    ce <- entry cp []
    set ce [on enterKey := emitCommand ce h]

    -- Games list
    glp <- panel nb []
    (gl, glHandler)  <- wxGamesList glp h

    -- about menu
    m_help    <- menuHelp      []
    _         <- menuAbout m_help [help := "About XChess", on command := wxAbout ]

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
    evtHandlerOnMenuCommand f ficsEventId $ takeMVar vCmd
      >>= glHandler >>= slHandler >>= pendingHandler >>= \cmd -> case cmd of

        NoSuchGame -> do
          set status [text := "No such game. Updating games..."]
          hPutStrLn h "4 games"

        TextMessage text -> appendText ct $ text ++ "\n"

        SettingsDone -> hPutStrLn h "4 iset seekinfo 1" >> hPutStrLn h "4 games"

        InvalidPassword  -> M.void $ set status [text := "Invalid password."]

        LoggedIn userName -> hPutStrLn h `mapM_` defaultParams >>
                             set f [ text := "MacBeth - Logged in as " ++ userName ] >>
                             set tbarItem_seek [ enabled := True ] >>
                             set tbarItem_match [ enabled := True ] >>
                             set tbarItem_finger [ enabled := True ]

        GuestLogin _ -> set tbarItem_seek  [on command := wxSeek h True ] >>
                        set tbarItem_match  [on command := wxMatch h True ]

        Finger name stats -> wxFinger name stats

        Login -> dupChan chan >>= wxLogin h

        Observe move -> dupChan chan >>= createObservedGame h move

        MatchAccepted move -> dupChan chan >>= createObservedGame h move

        GameMove move -> M.when (isNewGameUser move) $ dupChan chan >>= createObservedGame h move

        MatchRequested c -> dupChan chan >>= wxChallenge h c

        _ -> return ()


emitCommand :: TextCtrl () -> Handle -> IO ()
emitCommand textCtrl h = get textCtrl text >>= hPutStrLn h . ("5 " ++) >> set textCtrl [text := ""]


defaultParams = [ "set seek 0", "set style 12", "iset nowrap 1", "iset block 1"]

notebookPageChanged :: Notebook () -> (Graphics.UI.WXCore.Event () -> IO ()) -> IO ()
notebookPageChanged nb eventHandler =
  windowOnEvent nb [wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGED] eventHandler (\evt -> eventHandler evt)


