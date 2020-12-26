module Macbeth.Wx.ToolBox (
  wxToolBox
) where

import           Control.Concurrent
import           Control.Concurrent.Chan ()
import           Control.Concurrent.STM.TVar
import           Control.Lens hiding (set)
import           Control.Monad
import           Control.Monad.STM
import           Data.Maybe
import           Graphics.UI.WX hiding (when, play)
import           Graphics.UI.WXCore hiding (when)
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Offer
import           Macbeth.Fics.Api.Player
import qualified Macbeth.Fics.Api.Result as R
import           Macbeth.Wx.Configuration
import           Macbeth.Wx.Chat
import           Macbeth.Wx.ChatRegistry
import qualified Macbeth.Fics.Commands as Cmds
import           Macbeth.Wx.Finger
import qualified Macbeth.Wx.CommandHistory as History
import           Macbeth.Wx.GamesList
import           Macbeth.Wx.Icons (Icon(..))
import qualified Macbeth.Wx.Icons as Icons
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
import           Macbeth.Wx.RuntimeEnv (reIsAutoLogin, reCommandHistory)


eventId :: Int
eventId = wxID_HIGHEST + 1

wxToolBox :: E.RuntimeEnv -> Chan Message -> IO ()
wxToolBox env chan = do
    f  <- frame [ text := "Macbeth"]

    tbar   <- toolBarEx f True False []
    tbarItem_seek <- toolItem tbar "Seek" False (Icons.filepath BullhornIcon)
      [ on command := dupChan chan >>= seekFrame env False, enabled := False, tooltip := "Seek" ]

    tbarItem_match <- toolItem tbar "Match" False (Icons.filepath LightningIcon)
      [ on command := dupChan chan >>= wxMatch env False, enabled := False, tooltip := "Match" ]

    tbarItem_finger <- toolItem tbar "Finger" False (Icons.filepath UserSaysIcon)
      [ on command := Cmds.finger env Nothing, enabled := False, tooltip := "Finger"]

    tbarItem_history <- toolItem tbar "History" False (Icons.filepath ListIcon)
      [ on command := Cmds.history env Nothing, enabled := False, tooltip := "History"]

    tbarItem_settings <- toolItem tbar "Settings" False (Icons.filepath WrenchIcon)
      [ on command := dupChan chan >>= wxConfiguration env, enabled := False, tooltip := "Settings"]

    statusMsg <- statusField []
    statusLag <- statusField [ statusWidth := 100 ]
    statusLoggedIn <- statusField [ statusWidth := 100]
    pingTimer <- timer f [ interval := 5 * 60 * 1000, on command := Cmds.ping env, enabled := False]


    nb <- notebook f []

    -- Sought list
    slp <- panel nb []
    (sl, soughtListHandler) <- wxSoughtList slp env

    -- Games list
    glp <- panel nb []
    (gl, gamesListHandler)  <- wxGamesList glp env

    -- Pending
    pending <- panel nb []
    (pendingWidget, pendingHandler) <- wxPending env pending

    -- Stored / Adjourned
    stored <- panel nb []
    (storedWidget, storedHandler) <- Stored.widget env stored

    -- Players
    players <- panel nb []
    (playersWidget, playersHandler) <- wxPlayersList players env chan

    -- ChatRegistry
    chatRegistryHandler <- dupChan chan >>= wxChatRegistry env

    -- Console
    cp <- panel nb []
    ct <- textCtrlEx cp (wxTE_MULTILINE .+. wxTE_RICH .+. wxTE_READONLY) [font := fontFixed {_fontSize = env `E.getConfig` C.fontSize}]

    commandEntry <- entry cp []
    windowOnKeyDown commandEntry $ \evt -> if
      | onlyEnter evt -> do
          cmd <- get commandEntry text 
          Cmds.messageWithCommandId env cmd 
          atomically $ modifyTVar (env ^. reCommandHistory) $ History.push cmd
          set commandEntry [text := ""] 

      | onlyUp evt -> do
          mCmd <- atomically $ stateTVar (env ^. reCommandHistory) History.up
          set commandEntry [text := fromMaybe "" mCmd] 

      | onlyDown evt -> do
          mCmd <- atomically $ stateTVar (env ^. reCommandHistory) History.down
          set commandEntry [text := fromMaybe "" mCmd] 

      | otherwise -> propagateEvent
    

    set f [ statusBar := [statusMsg, statusLoggedIn, statusLag],
            layout := tabs nb
                        [ tab "Sought" $ container slp $ fill $ widget sl
                        , tab "Games" $ container glp $ fill $ widget gl
                        , tab "Pending" $ container pending $ fill $ widget pendingWidget
                        , tab "Stored" $ container stored $ fill $ widget storedWidget
                        , tab "Players" $ container players $ fill $ widget playersWidget
                        , tab "Console" $ container cp ( column 5  [ fill $ widget ct
                                                                   , hfill $ widget commandEntry])
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
          Cmds.games env 

        UserNotLoggedIn username -> set statusMsg [text := username ++ " is not logged in."]

        PartnerNotOpen userHandle -> set statusMsg [text := name userHandle ++ " is not open for bughouse."]

        PartnerOffer userHandle -> dupChan chan >>= wxPartnerOffer env userHandle

        PartnerAccepted userHandle -> set statusMsg [text := name userHandle ++ " agrees to be your partner."]

        PartnerDeclined userHandle -> set statusMsg [text := name userHandle ++ " declines the partnership request."]

        SeekNotAvailable -> set statusMsg [text := "That seek is not available."]

        InvalidPassword -> readTVarIO (env ^. reIsAutoLogin) >>= \case
          True -> do
             atomically $ writeTVar (env ^. reIsAutoLogin) False
             dupChan chan >>= wxLogin env
          False -> void $ set statusMsg [text := "Invalid password."]

        LoginPrompt -> readTVarIO (env ^. reIsAutoLogin) >>= \case 
          True -> Cmds.message env $ E.getLoginUsername env
          False -> dupChan chan >>= wxLogin env

        Password -> readTVarIO (env ^. reIsAutoLogin) >>= \case 
          True -> Cmds.message env $ E.getLoginPassword env
          False -> return ()

        GuestLogin _ -> readTVarIO (env ^. reIsAutoLogin) >>= \case 
          True -> Cmds.message env ""
          False -> return ()

        LoggedIn userHandle -> do
          E.playSound env (S.logonToServer . S.other)
          E.setUsername env userHandle
          unless (isGuest userHandle) $ Cmds.ping env >> set pingTimer [enabled := True]
          set nb [on click := (onMouse nb >=> clickHandler env nb)]
          Cmds.message env `mapM_` [ "set seek 0", "set style 12", "iset pendinfo 1", "iset seekinfo 1", "iset nowrap 1", "iset defprompt 1", "iset block 1", "2 iset lock 1"]
          set statusLoggedIn [ text := name userHandle]
          mapM_ (`set` [ enabled := True ]) [tbarItem_seek, tbarItem_match, tbarItem_finger, tbarItem_history, tbarItem_settings]

        LoginTimeout -> set statusMsg [ text := "Login Timeout." ]

        msg@Finger {} -> dupChan chan >>= wxInfo msg

        msg@History {} -> dupChan chan >>= wxInfo msg

        Challenge gameParams -> do
          E.playSound env (S.challenge . S.request)
          dupChan chan >>= wxChallenge env gameParams

        WxObserving gameId gameParams msgChan -> do
           E.playSound env (S.newGame . S.game)
           E.trackObservingGame env gameId
           wxGame env gameId gameParams False msgChan

        WxGame gameId gameParams msgChan -> do
           E.playSound env (S.newGame . S.game)
           E.trackOngoingGame env gameId
           wxGame env gameId gameParams True msgChan
 
        WxChat chatId -> do
          isTracked <- env `E.isTrackedChat` chatId
          unless isTracked $ do
            env `E.trackChat` chatId
            dupChan chan >>= wxChat env chatId Nothing

        Unobserving gameId -> E.untrackGame env gameId

        GameResult result -> E.untrackGame env $ R.gameId result

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
      History.save =<< readTVarIO (env ^. reCommandHistory)
      writeChan chan WxClose 
      killThread threadId


onMouse :: Notebook() -> Point -> IO Int
onMouse nb p = propagateEvent >> notebookHitTest nb p flag

clickHandler :: E.RuntimeEnv -> Notebook () -> Int -> IO ()
clickHandler env nb idx 
  | idx == -1 = return () -- click didn't happen on a notebook tab
  | otherwise = notebookGetPageText nb idx >>= \case
      "Games" -> Cmds.games env
      "Players" -> Cmds.who env
      "Stored" -> Cmds.stored env
      _ -> return ()


abusiveBehaviorMessage :: String
abusiveBehaviorMessage = "To be able to log in, please email: abuse@freechess.org\nWe are sorry about this and apologize for the inconvenience." 

connectionClosedMessage :: String
connectionClosedMessage = "Macbeth could not maintain the connection to FICS. It could be that FICS is down or that the connection was closed for other reasons. Please restart Macbeth."

