{-# LANGUAGE CPP #-}

module Macbeth.Wx.Game.Game (
  wxGame
) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           Graphics.UI.WX hiding (when, position, play, point, white, black)
import           Graphics.UI.WXCore hiding (when, Timer, black, white, point)
import           Macbeth.Fics.Message hiding (gameId)
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Chat
import           Macbeth.Fics.Api.Offer
import qualified Macbeth.Fics.Api.Move as M
import qualified Macbeth.Fics.Api.Game as G
import qualified Macbeth.Fics.Api.Result as R
import           Macbeth.Utils.PGN
import qualified Macbeth.Wx.Commands as Cmds
import           Macbeth.Wx.Game.PieceSet (PieceSet(..))
import qualified Macbeth.Wx.Game.PieceSet as PieceSet
import           Macbeth.Wx.Game.StatusPanel
import           Macbeth.Wx.Game.GameSounds
import           Macbeth.Wx.Config.BoardConfig
import qualified Macbeth.Wx.Config.UserConfig as UserConfig
import qualified Macbeth.Wx.Utils as Utl
import qualified Macbeth.Wx.RuntimeEnv as E
import qualified Macbeth.Wx.Game.BoardState as Api
import qualified Macbeth.Wx.Game.Board as Board
import           System.IO

eventId :: Int
eventId = wxID_HIGHEST + 1

-- some nasty layout fixes
windowResizeMargin :: Int
windowResizeMargin = 75

#ifdef darwin_HOST_OS
windowInitMargin :: Int
windowInitMargin = 115
#endif

#ifdef linux_HOST_OS
windowInitMargin :: Int
windowInitMargin = 125
#endif


wxGame :: E.RuntimeEnv -> GameId -> G.GameParams  -> Chan Message -> IO ()
wxGame env gameId gameParams' chan = do
  let h = E.handle env
  username' <- E.username env
  boardConfig <- readTVarIO $ E.rtBoardConfig env
  vCmd <- newEmptyMVar

  f <- frame [ text := G.toTitle gameId gameParams']
  p_back <- panel f []


  -- board
  let boardState = Api.initBoardState gameId gameParams' username' boardConfig env
  vBoardState <- newTVarIO boardState

  -- player panels
  (p_white, updateClockW) <- wxStatusPanel p_back White vBoardState
  (p_black, updateClockB) <- wxStatusPanel p_back Black vBoardState

  p_board <- panel p_back [ on paint := Board.draw vBoardState ]

  -- layout helper
  let updateBoardLayoutIO = updateBoardLayout p_back p_board p_white p_black vBoardState
  updateBoardLayoutIO

  status <- statusField []
  promotion <- statusField [ statusWidth := 30, text := "=Q"]

  -- context menu
  ctxMenu <- menuPane []

  void $ menuItem ctxMenu [ text := "Turn board", on command := Api.invertPerspective vBoardState >> updateBoardLayoutIO >> void (windowLayout f)]

  void $ menuItem ctxMenu [ text := "Show captured pieces", checkable:= True, checked := showCapturedPieces boardConfig,  on command :=
    atomically (modifyTVar vBoardState flipShowCapturedPieces) >> repaint p_back]

  when (G.isGameUser' gameParams') $ do
     menuLine ctxMenu
     void $ menuItem ctxMenu [ text := "Request takeback 1", on command := Cmds.takeback h 1 ]
     void $ menuItem ctxMenu [ text := "Request takeback 2", on command := Cmds.takeback h 2 ]
     void $ menuItem ctxMenu [ text := "Request abort", on command := Cmds.abort h ]
     void $ menuItem ctxMenu [ text := "Offer draw", on command := Cmds.draw h ]
     void $ menuItem ctxMenu [ text := "Resign", on command := Cmds.resign h ]
     menuLine ctxMenu
     void $ menuItem ctxMenu [ text := "Chat", on command := writeChan chan $ Chat $ OpenChat (fromMaybe "" $ G.nameOponent username' gameParams') (Just gameId)]

     windowOnMouse p_board True (\point -> Board.onMouseEvent h vBoardState point >> repaint p_board)
  menuLine ctxMenu
  wxPieceSetsMenu ctxMenu vBoardState p_board

  set p_board [ on clickRight := (\point -> menuPopup ctxMenu point p_board) ]

  -- key handler
  windowOnKeyDown p_board (\evt -> if
    | Utl.onlyKey evt 'X' -> Api.cancelLastPreMove vBoardState >> repaint p_board

    | Utl.onlyKey evt 'Q' -> Api.pickUpPieceFromHolding vBoardState Queen >> repaint p_board
    | Utl.onlyKey evt 'B' -> Api.pickUpPieceFromHolding vBoardState Bishop >> repaint p_board
    | Utl.onlyKey evt 'K' -> Api.pickUpPieceFromHolding vBoardState Knight >> repaint p_board
    | Utl.onlyKey evt 'R' -> Api.pickUpPieceFromHolding vBoardState Rook >> repaint p_board
    | Utl.onlyKey evt 'P' -> Api.pickUpPieceFromHolding vBoardState Pawn >> repaint p_board
    | Utl.onlyKey evt 'T' && G.isGameUser' gameParams' -> writeChan chan $ Chat $ OpenChat (fromMaybe "" $ G.nameOponent username' gameParams') (Just gameId)
    | (keyKey evt == KeyEscape) && isNoneDown (keyModifiers evt) -> Api.discardDraggedPiece vBoardState >> repaint p_board

    | Utl.onlyKey evt 'N' -> Cmds.decline h
    | Utl.onlyKey evt 'Y' -> Cmds.accept h

    | Utl.keyWithMod evt 'W' justControl -> close f
    | Utl.keyWithMod evt 'O' justControl -> Api.togglePromotion vBoardState >>= \p -> set promotion [text := "=" ++ show p]
    | otherwise -> return ())

  windowOnKeyUp p_board $ onKeyUpHandler vBoardState h promotion

  --set layout
  set f [ statusBar := [status] ++ [promotion | Api.isGameUser boardState]
        , layout := fill $ widget p_back
        , size := Size (boardSize boardConfig) (boardSize boardConfig + windowInitMargin)
        , on resize := resizeFrame f vBoardState ]

  -- necessary: after GameResult no more events are handled
  tiClose <- dupChan chan >>= Utl.registerWxCloseEventListenerWithThreadId f

  threadId <- forkIO $ Utl.eventLoop eventId chan vCmd f
  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd ->
    updateClockW cmd >> updateClockB cmd >> gameSounds env boardState cmd >> case cmd of

    GameMove ctx move' -> when (M.gameId move' == gameId) $ do
      set status [text := show ctx]
      Api.update vBoardState move' ctx
      when (M.isNextMoveUser move') $ Api.performPreMoves vBoardState h
      repaint p_back

    GameResult result -> when (R.gameId result == gameId) $ do
      set status [text := R.toString result]
      Api.setResult vBoardState (R.result result)
      repaint p_board
      when (Api.isGameUser boardState) $ readTVarIO vBoardState >>= saveAsPGN
      hPutStrLn h "4 iset seekinfo 1"
      killThread threadId

    DrawRequest user' -> set status [text := user' ++ " offered a draw. Accept? (y/n)"]

    AbortRequest user' -> set status [text := user' ++ " would like to abort the game. Accept? (y/n)"]

    TakebackRequest user' numTakeback -> set status [text := user' ++ " would like to take back " ++ show numTakeback ++ " half move(s). Accept? (y/n)"]

    OponentDecline user' sub
      | sub `elem` [DrawReq, TakebackReq, AbortReq] -> set status [text := user' ++ " declines the " ++ show sub ++ " request."]
      | otherwise -> return ()

    PromotionPiece p -> Api.setPromotion p vBoardState >> set promotion [text := "=" ++ show p]

    PieceHolding id' phW' phB' -> when (id' == gameId) $ do
      atomically $ modifyTVar vBoardState (\s -> s{ Api.phW = phW', Api.phB = phB' })
      repaint p_white
      repaint p_black

    _ -> return ()

  windowOnDestroy f $ do
    sequence_ $ fmap (handle (\(_ :: IOException) -> return ()) . killThread) [threadId, tiClose]
    boardState' <- readTVarIO vBoardState

    config <- UserConfig.loadConfig
    let boardConfigFormat = UserConfig.boardConfig config
    let updated = config { 
      UserConfig.boardConfig = (\s -> s { 
        boardSize = Just $ boardSize $ Api.boardConfig boardState'
      , showCapturedPieces = showCapturedPieces $ Api.boardConfig boardState'
      , pieceSet = Just $ pieceSet $ Api.boardConfig boardState'
      }) <$> boardConfigFormat}
    UserConfig.saveConfig updated
    u <- convert (fromMaybe defaultBoardConfig $ UserConfig.boardConfig updated) (UserConfig.directory config)
    E.setBoardConfig env u

    when (isNothing (Api.gameResult boardState') && not (Api.isGameUser boardState)) $ Cmds.unobserve h gameId


resizeFrame :: Frame () -> TVar Api.BoardState -> IO ()
resizeFrame f vBoardState = do
  (Size w h) <- windowGetClientSize f
  let newBoardSize = min w (h - windowResizeMargin)
  Api.resize vBoardState newBoardSize
#ifdef darwin_HOST_OS
  windowSetClientSize f $ Size newBoardSize (newBoardSize+windowResizeMargin)
#endif
  void $ windowLayout f


updateBoardLayout :: Panel() -> Panel() -> Panel() -> Panel() -> TVar Api.BoardState -> IO ()
updateBoardLayout pback board white black vBoardState = do
  state <- readTVarIO vBoardState
  set pback [ layout := column 0 [ marginWidth 5 $ marginTop $ hfill $ widget (if Api.perspective state == White then black else white)
                                 , stretch $ shaped $ widget board
                                 , marginWidth 5 $ marginTop $ hfill $ widget (if Api.perspective state == White then white else black)]]


wxPieceSetsMenu :: Menu () -> TVar Api.BoardState -> Panel () -> IO ()
wxPieceSetsMenu ctxMenu vState p = do
  sub <- menuPane [text := "Piece Sets"]
  mapM_ (\ps -> menuItem sub [ text := PieceSet.display ps, on command := Api.setPieceSet vState ps >> repaint p ]) $ enumFrom Alpha1
  void $ menuSub ctxMenu sub [ text := "Piece Sets" ]


onKeyUpHandler :: TVar Api.BoardState -> Handle -> StatusField -> EventKey -> IO ()
onKeyUpHandler vBoardState h sf evt
  | (keyKey evt == KeyControl) && isNoneDown (keyModifiers evt) =
      Api.promotion `fmap` readTVarIO vBoardState >>= \p -> do
        Cmds.promote h p
        set sf [text := "=" ]
  | otherwise = return ()

flipShowCapturedPieces :: Api.BoardState -> Api.BoardState
flipShowCapturedPieces boardState = 
  let boardConfig' = Api.boardConfig boardState
      flipped = boardConfig' { showCapturedPieces = not $ showCapturedPieces boardConfig' }
  in boardState { Api.boardConfig = flipped }
