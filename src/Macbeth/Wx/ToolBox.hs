{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Macbeth.Wx.ToolBox (
  wxToolBox
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Offer
import Macbeth.Fics.Api.Player
import Macbeth.Utils.Utils
import Macbeth.Wx.Configuration
import Macbeth.Wx.ChatRegistry
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
import qualified Macbeth.Wx.Config.UserConfig as C
import qualified Macbeth.Wx.RuntimeEnv as E

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Graphics.UI.WX hiding (when, play)
import Graphics.UI.WXCore hiding (when)
import System.IO
import System.IO.Unsafe


eventId :: Int
eventId = wxID_HIGHEST + 1

wxToolBox :: E.RuntimeEnv -> Chan FicsMessage -> IO ()
wxToolBox env chan = do
    let h = E.handle env
    f  <- frame [ text := "Macbeth"]

    tbar   <- toolBarEx f True False []
    tbarItem_seek <- toolItem tbar "Seek" False (E.getIconFilePath "bullhorn")
      [ on command := dupChan chan >>= wxSeek h False, enabled := False, tooltip := "Seek" ]

    tbarItem_match <- toolItem tbar "Match" False (E.getIconFilePath "match")
      [ on command := dupChan chan >>= wxMatch h False, enabled := False, tooltip := "Match" ]

    tbarItem_finger <- toolItem tbar "Finger" False (E.getIconFilePath "fa-question")
      [ on command := hPutStrLn h "4 finger", enabled := False, tooltip := "Finger"]

    tbarItem_history <- toolItem tbar "History" False (E.getIconFilePath "history")
      [ on command := hPutStrLn h "4 history", enabled := False, tooltip := "History"]

    tbarItem_settings <- toolItem tbar "Settings" False (E.getIconFilePath "settings")
      [ on command := dupChan chan >>= wxConfiguration env, enabled := False, tooltip := "Settings"]

    statusMsg <- statusField []
    statusLag <- statusField [ statusWidth := 100 ]
    pingTimer <- timer f [ interval := 5 * 60 * 1000, on command := hPutStrLn h "4 ping", enabled := False]
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
    (playersWidget, playersHandler) <- wxPlayersList players h chan

    -- ChatRegistry
    chatRegistryHandler <- dupChan chan >>= wxChatRegistry env

    -- Console
    cp <- panel nb []
    ct <- textCtrlEx cp (wxTE_MULTILINE .+. wxTE_RICH .+. wxTE_READONLY) [font := fontFixed {_fontSize = env `E.getConfig` C.fontSize}]
    ce <- entry cp []
    set ce [on enterKey := emitCommand ce h]

    set f [ statusBar := [statusMsg, statusLoggedIn, statusLag],
            layout := tabs nb
                        [ tab "Sought" $ container slp $ fill $ widget sl
                        , tab "Games" $ container glp $ fill $ widget gl
                        , tab "Pending" $ container pending $ fill $ widget pendingWidget
                        , tab "Players" $ container players $ fill $ widget playersWidget
                        , tab "Console" $ container cp ( column 5  [ fill $ widget ct
                                                                   , hfill $ widget ce])
                        ]
          , outerSize := sz 650 600
          ]

    -- preselect first tab
    _ <- notebookSetSelection nb 0

    vCmd <- newEmptyMVar
    threadId <- forkIO $ eventLoop eventId chan vCmd f
    windowOnDestroy f $ writeChan chan WxClose >> killThread threadId

    evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd ->
      gamesListHandler cmd >>
      soughtListHandler cmd >>
      pendingHandler cmd >>
      playersHandler cmd >>
      chatRegistryHandler cmd >>
      case cmd of

        NoSuchGame -> do
          set statusMsg [text := "No such game. Updating games..."]
          hPutStrLn h "4 games"

        UserNotLoggedIn username -> set statusMsg [text := username ++ " is not logged in."]

        PartnerNotOpen userHandle -> set statusMsg [text := name userHandle ++ " is not open for bughouse."]

        PartnerOffer userHandle -> dupChan chan >>= wxPartnerOffer h userHandle

        PartnerAccepted userHandle -> set statusMsg [text := name userHandle ++ " agrees to be your partner."]

        PartnerDeclined userHandle -> set statusMsg [text := name userHandle ++ " declines the partnership request."]

        SeekNotAvailable -> set statusMsg [text := "That seek is not available."]

        InvalidPassword  -> void $ set statusMsg [text := "Invalid password."]

        LoginTimeout -> set statusMsg [ text := "Login Timeout." ]

        LoginPrompt | (env `E.getConfig` C.autologin) -> hPutStrLn h (maybe (error "autologin: username not set") C.username (env `E.getConfig` C.user))
                    | otherwise -> dupChan chan >>= showLogin h

        Password -> when (env `E.getConfig` C.autologin) $
          hPutStrLn h (maybe (error "autologin: password not set") (decrypt . C.password) (env `E.getConfig` C.user))

        GuestLogin _ -> do
          when (env `E.getConfig` C.autologin) $ hPutStrLn h ""
          set tbarItem_seek  [on command := dupChan chan >>= wxSeek h True ]
          set tbarItem_match  [on command := dupChan chan >>= wxMatch h True ]

        LoggedIn userHandle -> do
          E.playSound env (C.logonToServer . C.other)
          E.setUsername env userHandle
          unless (isGuest userHandle) $ hPutStrLn h "4 ping" >> set pingTimer [enabled := True]
          set nb [on click := (onMouse nb >=> clickHandler h nb)]
          hPutStrLn h `mapM_` [ "set seek 0", "set style 12", "iset pendinfo 1", "iset seekinfo 1", "iset nowrap 1", "iset defprompt 1", "iset block 1", "2 iset lock 1"]
          set statusLoggedIn [ text := name userHandle]
          mapM_ (`set` [ enabled := True ]) [tbarItem_seek, tbarItem_match, tbarItem_finger, tbarItem_history, tbarItem_settings]

        msg@Finger {} -> dupChan chan >>= wxInfo msg

        msg@History {} -> dupChan chan >>= wxInfo msg

        MatchRequested c -> do
          E.playSound env (C.challenge . C.request)
          dupChan chan >>= wxChallenge h c

        WxOpenBoard gameId' gameParams' chain' -> do
           E.playSound env (C.newGame . C.game)
           wxGame env gameId' gameParams' chain'

        OponentDecline user MatchReq -> set statusMsg [text := user ++ " declines the match offer."]

        Ping _ avg _ -> set statusLag [ text := "Lag: " ++ show avg ++ "ms"]

        TextMessage text' -> appendText ct text'

        _ -> return ()


emitCommand :: TextCtrl () -> Handle -> IO ()
emitCommand tc h = get tc text >>= hPutStrLn h . ("5 " ++) >> set tc [text := ""]


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
             work <- malloc :: IO (Ptr CInt)
             poke work (fromIntegral wxBK_HITTEST_ONPAGE)
             return work
