module Macbeth.Wx.ToolBox (
  wxToolBox
) where

import           Control.Concurrent
import           Control.Concurrent.Chan ()
import           Control.Concurrent.STM.TVar
import           Control.Lens hiding (set)
import           Control.Monad
import           Control.Monad.STM
import           Graphics.UI.WX hiding (when, play)
import           Graphics.UI.WX.Dialogs (errorDialog)
import           Graphics.UI.WXCore hiding (when)
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Offer
import           Macbeth.Fics.Api.Player
import           Macbeth.Wx.Configuration
import           Macbeth.Wx.ChatRegistry
import qualified Macbeth.Wx.Commands as Cmds
import qualified Macbeth.Wx.Dialog as Dialog
import           Macbeth.Wx.Finger
import           Macbeth.Wx.GamesList
import           Macbeth.Wx.Login
import           Macbeth.Wx.Match
import           Macbeth.Wx.Seek
import qualified Macbeth.Wx.Stored as Stored
import           Macbeth.Wx.Utils
import           Macbeth.Wx.PlayersList
import           Macbeth.Wx.SoughtList
import           Macbeth.Wx.Game.Game
import           Macbeth.Wx.Challenge
import           Macbeth.Wx.PartnerOffer
import           Macbeth.Wx.Pending
import qualified Macbeth.Wx.Config.UserConfig as C
import qualified Macbeth.Wx.Config.Sounds as S
import qualified Macbeth.Wx.RuntimeEnv as E
import           Macbeth.Wx.RuntimeEnv (reIsAutoLogin)
import           System.IO


eventId :: Int
eventId = wxID_HIGHEST + 1

wxToolBox :: E.RuntimeEnv -> Chan Message -> IO ()
wxToolBox env chan = do
    let h = E.handle env
    f  <- frame [ text := "Macbeth"]

    tbar   <- toolBarEx f True False []
    tbarItem_seek <- toolItem tbar "Seek" False (E.getIconFilePath "bullhorn")
      [ on command := dupChan chan >>= seekFrame env False, enabled := False, tooltip := "Seek" ]

    tbarItem_match <- toolItem tbar "Match" False (E.getIconFilePath "match")
      [ on command := dupChan chan >>= wxMatch h False, enabled := False, tooltip := "Match" ]

    tbarItem_finger <- toolItem tbar "Finger" False (E.getIconFilePath "fa-question")
      [ on command := Cmds.finger h Nothing, enabled := False, tooltip := "Finger"]

    tbarItem_history <- toolItem tbar "History" False (E.getIconFilePath "history")
      [ on command := Cmds.history h Nothing, enabled := False, tooltip := "History"]

    tbarItem_settings <- toolItem tbar "Settings" False (E.getIconFilePath "settings")
      [ on command := dupChan chan >>= wxConfiguration env, enabled := False, tooltip := "Settings"]

    statusMsg <- statusField []
    statusLag <- statusField [ statusWidth := 100 ]
    pingTimer <- timer f [ interval := 5 * 60 * 1000, on command := Cmds.ping h, enabled := False]
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

    -- Stored / Adjourned
    stored <- panel nb []
    (storedWidget, storedHandler) <- Stored.widget h stored

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
                        , tab "Stored" $ container stored $ fill $ widget storedWidget
                        , tab "Players" $ container players $ fill $ widget playersWidget
                        , tab "Console" $ container cp ( column 5  [ fill $ widget ct
                                                                   , hfill $ widget ce])
                        ]
          ]

    -- preselect first tab
    void $ notebookSetSelection nb 0

    threadId <- eventLoop f eventId chan $ \cmd -> do
      gamesListHandler cmd
      soughtListHandler cmd
      pendingHandler cmd
      storedHandler cmd
      playersHandler cmd
      chatRegistryHandler cmd
      case cmd of

        NoSuchGame -> do
          set statusMsg [text := "No such game. Updating games..."]
          Cmds.games h 

        UserNotLoggedIn username -> set statusMsg [text := username ++ " is not logged in."]

        PartnerNotOpen userHandle -> set statusMsg [text := name userHandle ++ " is not open for bughouse."]

        PartnerOffer userHandle -> dupChan chan >>= wxPartnerOffer h userHandle

        PartnerAccepted userHandle -> set statusMsg [text := name userHandle ++ " agrees to be your partner."]

        PartnerDeclined userHandle -> set statusMsg [text := name userHandle ++ " declines the partnership request."]

        SeekNotAvailable -> set statusMsg [text := "That seek is not available."]

        InvalidPassword -> readTVarIO (env ^. reIsAutoLogin) >>= \case
          True -> do
             atomically $ writeTVar (env ^. reIsAutoLogin) False
             dupChan chan >>= wxLogin h
          False -> void $ set statusMsg [text := "Invalid password."]

        LoginPrompt -> readTVarIO (env ^. reIsAutoLogin) >>= \case 
          True -> hPutStrLn h $ E.getLoginUsername env
          False -> dupChan chan >>= wxLogin h

        Password -> readTVarIO (env ^. reIsAutoLogin) >>= \case 
          True -> hPutStrLn h $ E.getLoginPassword env
          False -> return ()

        GuestLogin _ -> readTVarIO (env ^. reIsAutoLogin) >>= \case 
          True -> hPutStrLn h ""
          False -> return ()

        LoggedIn userHandle -> do
          E.playSound env (S.logonToServer . S.other)
          E.setUsername env userHandle
          unless (isGuest userHandle) $ Cmds.ping h >> set pingTimer [enabled := True]
          set nb [on click := (onMouse nb >=> clickHandler h nb)]
          hPutStrLn h `mapM_` [ "set seek 0", "set style 12", "iset pendinfo 1", "iset seekinfo 1", "iset nowrap 1", "iset defprompt 1", "iset block 1", "2 iset lock 1"]
          set statusLoggedIn [ text := name userHandle]
          mapM_ (`set` [ enabled := True ]) [tbarItem_seek, tbarItem_match, tbarItem_finger, tbarItem_history, tbarItem_settings]

        LoginTimeout -> set statusMsg [ text := "Login Timeout." ]

        msg@Finger {} -> dupChan chan >>= wxInfo msg

        msg@History {} -> dupChan chan >>= wxInfo msg

        Challenge gameParams -> do
          E.playSound env (S.challenge . S.request)
          dupChan chan >>= wxChallenge h gameParams

        WxObserving gameId gameParams chan -> do
           E.playSound env (S.newGame . S.game)
           wxGame env gameId gameParams False chan

        WxGame gameId gameParams chan -> do
           E.playSound env (S.newGame . S.game)
           wxGame env gameId gameParams True chan

        OponentDecline user MatchReq -> set statusMsg [text := user ++ " declines the match offer."]

        Ping _ avg _ -> set statusLag [ text := "Lag: " ++ show avg ++ "ms"]

        ConnectionClosed _ -> errorDialog f "FICS Connection Error" connectionClosedMessage

        AbusiveBehavior -> errorDialog f "Abusive Behavior" abusiveBehaviorMessage

        TextMessage msg -> appendText ct msg

        _ -> return ()



    windowOnClose f $ do
      result <- confirmDialog f "Macbeth" "Do you really want to quit Macbeth?" True
      when result $ void $ windowDestroy f
    

    windowOnDestroy f $ do
      writeChan chan WxClose 
      killThread threadId


emitCommand :: TextCtrl () -> Handle -> IO ()
emitCommand tc h = get tc text >>= hPutStrLn h . ("5 " ++) >> set tc [text := ""]


onMouse :: Notebook() -> Point -> IO Int
onMouse nb p = propagateEvent >> notebookHitTest nb p flag

clickHandler :: Handle -> Notebook () -> Int -> IO ()
clickHandler h nb idx 
  | idx == -1 = return () -- click didn't happen on a notebook tab
  | otherwise = notebookGetPageText nb idx >>= \case
      "Games" -> Cmds.games h
      "Players" -> Cmds.who h
      "Stored" -> Cmds.stored h
      _ -> return ()


abusiveBehaviorMessage :: String
abusiveBehaviorMessage = "To be able to log in, please email: abuse@freechess.org\nWe are sorry about this and apologize for the inconvenience." 

connectionClosedMessage :: String
connectionClosedMessage = "Macbeth could not maintain the connection to FICS. It could be that FICS is down or that the connection was closed for other reasons. Please restart Macbeth."

