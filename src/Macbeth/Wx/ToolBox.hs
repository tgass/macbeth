{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Macbeth.Wx.ToolBox (
  wxToolBox
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Player
import Macbeth.Utils.Utils
import Macbeth.Wx.Configuration
import Macbeth.Wx.Finger
import Macbeth.Wx.GamesList
import Macbeth.Wx.Login
import Macbeth.Wx.Match
import Macbeth.Wx.Seek
import Macbeth.Wx.Utils
import Macbeth.Wx.PlayersList
import Macbeth.Wx.SoughtList
import Macbeth.Wx.Game.Game
import Macbeth.Wx.Challenge
import Macbeth.Wx.PartnerOffer
import Macbeth.Wx.Pending
import Macbeth.Wx.Sounds
import qualified Macbeth.Wx.Config.UserConfig as C
import Paths

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Graphics.UI.WX hiding (when, play)
import Graphics.UI.WXCore hiding (when)
import System.FilePath
import System.IO
import System.IO.Unsafe

eventId = wxID_HIGHEST + 1

wxToolBox :: Handle -> Chan FicsMessage -> Sounds -> IO ()
wxToolBox h chan sounds = do
    config <- C.initConfig
    f  <- frame [ text := "Macbeth"]

    tbar   <- toolBarEx f False False []
    tbarItem_seek <- toolItem tbar "Seek" False (unsafePerformIO $ getDataFileName $ "icons" </> "bullhorn.gif")
      [ on command := dupChan chan >>= wxSeek h False, enabled := False, tooltip := "Seek" ]

    tbarItem_match <- toolItem tbar "Match" False  (unsafePerformIO $ getDataFileName $ "icons" </> "dot-circle-o.gif")
      [ on command := dupChan chan >>= wxMatch h False, enabled := False, tooltip := "Match" ]

    tbarItem_finger <- toolItem tbar "Finger" False  (unsafePerformIO $ getDataFileName $ "icons" </> "fa-question.gif")
      [ on command := hPutStrLn h "4 finger", enabled := False, tooltip := "Finger"]

    status <- statusField []
    statusLag <- statusField [ statusWidth := 100 ]
    _ <- timer f [ interval := 5 * 60 * 1000, on command := hPutStrLn h "4 ping"]
    statusLoggedIn <- statusField [ statusWidth := 100]


    nb <- notebook f []

    -- Sought list
    slp <- panel nb []
    (sl, soughtListHandler) <- wxSoughtList slp h

    -- Games list
    glp <- panel nb []
    (gl, gamesListHandler)  <- wxGamesList glp h

    -- Pending
    pending <- panel nb []
    (pendingWidget, pendingHandler) <- wxPending h pending

    -- Players
    players <- panel nb []
    (playersWidget, playersHandler) <- wxPlayersList players h

    -- Console
    cp <- panel nb []
    ct <- textCtrlEx cp (wxTE_MULTILINE .+. wxTE_RICH .+. wxTE_READONLY) [font := fontFixed {_fontSize = C.fontSize config}]
    ce <- entry cp []
    set ce [on enterKey := emitCommand ce h]


    -- menu
    m_file   <- menuPane [text := "&File"]
    menuItem m_file [text := "Settings", on command := dupChan chan >>= wxConfiguration ]

    set f [ statusBar := [status, statusLoggedIn, statusLag],
            layout := tabs nb
                        [ tab "Sought" $ container slp $ fill $ widget sl
                        , tab "Games" $ container glp $ fill $ widget gl
                        , tab "Pending" $ container pending $ fill $ widget pendingWidget
                        , tab "Players" $ container players $ fill $ widget playersWidget
                        , tab "Console" $ container cp ( column 5  [ fill $ widget ct
                                                                   , hfill $ widget ce])
                        ]
          , menuBar := [m_file]
          , outerSize := sz 600 600
          ]

    -- preselect first tab
    notebookSetSelection nb 0

    vCmd <- newEmptyMVar
    threadId <- forkIO $ eventLoop eventId chan vCmd f
    windowOnDestroy f $ writeChan chan WxClose >> killThread threadId

    evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd ->
      gamesListHandler cmd >>
      soughtListHandler cmd >>
      pendingHandler cmd >>
      playersHandler cmd >>
      case cmd of

        NoSuchGame -> do
          set status [text := "No such game. Updating games..."]
          hPutStrLn h "4 games"

        UserNotLoggedIn username -> do
          set status [text := username ++ " is not logged in."]
          hPutStrLn h "4 who"

        PartnerNotOpen userHandle -> set status [text := name userHandle ++ " is not open for bughouse."]

        PartnerOffer userHandle -> dupChan chan >>= wxPartnerOffer h userHandle

        PartnerAccepted userHandle -> set status [text := name userHandle ++ " agrees to be your partner."]

        PartnerDeclined userHandle -> set status [text := name userHandle ++ " declines the partnership request."]

        SeekNotAvailable -> set status [text := "That seek is not available."]

        InvalidPassword  -> void $ set status [text := "Invalid password."]

        LoginTimeout -> set status [ text := "Login Timeout." ]

        Login | C.autologin config -> hPutStrLn h (maybe (error "autologin: username not set") C.username $ C.user config)
              | otherwise -> dupChan chan >>= wxLogin h

        Password -> when (C.autologin config) $
          hPutStrLn h (maybe (error "autologin: password not set") (decrypt . C.password) (C.user config))

        GuestLogin _ -> do
          when (C.autologin config) $ hPutStrLn h ""
          set tbarItem_seek  [on command := dupChan chan >>= wxSeek h True ]
          set tbarItem_match  [on command := dupChan chan >>= wxMatch h True ]

        LoggedIn handle -> do
          playSound (C.sounds config) (C.sounds config >>= C.logonToServer . C.other) sounds
          set nb [on click := (onMouse nb >=> clickHandler h nb)]
          hPutStrLn h `mapM_` [ "ping", "set seek 0", "set style 12", "iset pendinfo 1", "iset seekinfo 1", "iset nowrap 1", "iset defprompt 1", "iset block 1", "2 iset lock 1"]
          set statusLoggedIn [ text := name handle]
          mapM_ (`set` [ enabled := True ]) [tbarItem_seek, tbarItem_match, tbarItem_finger]

        msg@Finger {} -> dupChan chan >>= wxInfo msg

        msg@History {} -> dupChan chan >>= wxInfo msg

        WxObserve move chan' -> wxGame h move chan' sounds

        MatchRequested c -> do
          playSound (C.sounds config) (C.sounds config >>= C.challenge . C.request) sounds
          dupChan chan >>= wxChallenge h c

        WxMatchAccepted move chan' -> do
          playSound (C.sounds config) (C.sounds config >>= C.newGame . C.game) sounds
          wxGame h move chan' sounds

        MatchDeclined user -> set status [text := user ++ " declines the match offer."]

        MatchUserNotLoggedIn user -> set status [text := user ++ " not logged in."]

        Ping _ avg _ -> set statusLag [ text := "Lag: " ++ show avg ++ "ms"]

        TextMessage text -> appendText ct text

        _ -> return ()


emitCommand :: TextCtrl () -> Handle -> IO ()
emitCommand textCtrl h = get textCtrl text >>= hPutStrLn h . ("5 " ++) >> set textCtrl [text := ""]


onMouse :: Notebook() -> Point -> IO Int
onMouse nb p = propagateEvent >> notebookHitTest nb p flag

clickHandler :: Handle -> Notebook () -> Int -> IO ()
clickHandler h nb idx = notebookGetPageText nb idx >>= \case
  "Games" -> hPutStrLn h "5 games"
  "Players" -> hPutStrLn h "5 who"
  _ -> return ()


{-# NOINLINE flag #-}
flag :: Ptr CInt
flag  =  unsafePerformIO flag'
  where flag' = do
             work <- malloc::IO (Ptr CInt)
             poke work (fromIntegral wxBK_HITTEST_ONPAGE)
             return work
