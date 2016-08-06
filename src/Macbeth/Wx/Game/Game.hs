{-# LANGUAGE LambdaCase, ScopedTypeVariables, MultiWayIf #-}

module Macbeth.Wx.Game.Game (
  wxGame
) where

import Macbeth.Fics.FicsMessage hiding (gameId, Observing)
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Chat
import Macbeth.Fics.Api.Move
import Macbeth.Utils.PGN
import Macbeth.Wx.Game.PieceSet
import Macbeth.Wx.Game.StatusPanel
import Macbeth.Wx.Game.GameSounds
import qualified Macbeth.Fics.Api.Result as R
import qualified Macbeth.Wx.Utils as Utl
import qualified Macbeth.Wx.RuntimeEnv as E
import qualified Macbeth.Wx.Game.BoardState as Api
import qualified Macbeth.Wx.Game.Board as Board

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Maybe
import Graphics.UI.WX hiding (when, position, play)
import Graphics.UI.WXCore hiding (when, Timer)
import System.IO

eventId = wxID_HIGHEST + 1

wxGame :: E.RuntimeEnv -> Move -> Chan FicsMessage -> IO ()
wxGame env move chan = do
  let h = E.handle env
  vCmd <- newEmptyMVar

  f <- frame [ text := "[Game " ++ show (gameId move) ++ "] " ++ nameW move ++ " vs " ++ nameB move]
  p_back <- panel f []

  -- board
  let boardState = Api.initBoardState move
  vBoardState <- newTVarIO boardState
  p_board <- panel p_back [ on paint := Board.draw vBoardState]

  -- player panels
  (p_white, updateClockW) <- createStatusPanel p_back White vBoardState
  (p_black, updateClockB) <- createStatusPanel p_back Black vBoardState

  -- layout helper
  let updateBoardLayoutIO = updateBoardLayout p_back p_board p_white p_black vBoardState >> refit f
  updateBoardLayoutIO

  -- status line
  status <- statusField []
  promotion <- statusField [ statusWidth := 30, text := "=" ++ show (Api.promotion boardState)]

  -- context menu
  ctxMenu <- menuPane []
  menuItem ctxMenu [ text := "Turn board", on command :=
    Api.invertPerspective vBoardState >> updateBoardLayoutIO >> repaint p_board >> resizeFrame f vBoardState p_board]
  when (isGameUser move) $ do
     menuLine ctxMenu
     menuItem ctxMenu [ text := "Request takeback 1", on command := hPutStrLn h "4 takeback 1"]
     menuItem ctxMenu [ text := "Request takeback 2", on command := hPutStrLn h "4 takeback 2"]
     menuItem ctxMenu [ text := "Request abort", on command := hPutStrLn h "4 abort"]
     menuItem ctxMenu [ text := "Offer draw", on command := hPutStrLn h "4 draw" ]
     menuItem ctxMenu [ text := "Resign", on command := hPutStrLn h "4 resign" ]
     menuLine ctxMenu
     menuItem ctxMenu [ text := "Chat", on command := writeChan chan $ Chat $ OpenChat (nameOponent move) (Just $ gameId move)]
     windowOnMouse p_board True (\pt -> Board.onMouseEvent h vBoardState pt >> repaint p_board)

  menuLine ctxMenu
  wxPieceSetsMenu ctxMenu vBoardState p_board

  set p_board [ on clickRight := (\pt -> menuPopup ctxMenu pt p_board) ]

  -- key handler
  windowOnKeyDown p_board (\evt -> if
    | Utl.onlyKey evt 'X' -> do Api.cancelLastPreMove vBoardState; repaint p_board

    | Utl.onlyKey evt 'Q' -> do Api.pickUpPieceFromHolding vBoardState Queen; repaint p_board
    | Utl.onlyKey evt 'B' -> do Api.pickUpPieceFromHolding vBoardState Bishop; repaint p_board
    | Utl.onlyKey evt 'K' -> do Api.pickUpPieceFromHolding vBoardState Knight; repaint p_board
    | Utl.onlyKey evt 'R' -> do Api.pickUpPieceFromHolding vBoardState Rook; repaint p_board
    | Utl.onlyKey evt 'P' -> do Api.pickUpPieceFromHolding vBoardState Pawn; repaint p_board
    | Utl.onlyKey evt 'T' -> writeChan chan $ Chat $ OpenChat (nameOponent move) (Just $ gameId move)
    | (keyKey evt == KeyEscape) && isNoneDown (keyModifiers evt) -> do Api.discardDraggedPiece vBoardState; repaint p_board

    | Utl.onlyKey evt 'N' -> hPutStrLn h "5 decline"
    | Utl.onlyKey evt 'Y' -> hPutStrLn h "5 accept"

    | Utl.keyWithMod evt 'W' justControl -> close f
    | Utl.keyWithMod evt 'O' justControl -> Api.togglePromotion vBoardState >>= \p -> set promotion [text := "=" ++ show p]
    | otherwise -> return ())

  windowOnKeyUp p_board $ onKeyUpHandler vBoardState h promotion

  --set layout
  set f [ statusBar := [status] ++ [promotion | isGameUser move]
        , layout := fill $ widget p_back
        , on resize := resizeFrame f vBoardState p_board]

  -- necessary: after GameResult no more events are handled
  tiClose <- dupChan chan >>= Utl.registerWxCloseEventListenerWithThreadId f

  threadId <- forkIO $ Utl.eventLoop eventId chan vCmd f
  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd ->
    updateClockW cmd >> updateClockB cmd >> gameSounds env move cmd >> case cmd of

    GameMove ctx move' -> when (gameId move' == gameId move) $ do
      set status [text := show ctx]
      Api.update vBoardState move' ctx
      when (isNextMoveUser move') $ Api.performPreMoves vBoardState h
      repaint p_board

    GameResult result -> when (R.gameId result == gameId move) $ do
      set status [text := R.toString result]
      Api.setResult vBoardState (R.result result)
      repaint p_board
      hPutStrLn h "4 iset seekinfo 1"
      killThread threadId

    DrawRequest -> set status [text := nameOponent move ++ " offered a draw. Accept? (y/n)"]

    DrawRequestDeclined user -> set status [text := user ++ " declines the draw request."]

    AbortRequest user -> set status [text := user ++ " would like to abort the game. Accept? (y/n)"]

    AbortRequestDeclined user -> set status [text := user ++ " declines the abort request."]

    TakebackRequest user numTakeback -> set status [text := user ++ " would like to take back " ++ show numTakeback ++ " half move(s). Accept? (y/n)"]

    TakebackRequestDeclined user -> set status [text := user ++ " declines the takeback request."]

    PromotionPiece p -> Api.setPromotion p vBoardState >> set promotion [text := "=" ++ show p]

    _ -> return ()

  windowOnDestroy f $ do
    sequence_ $ fmap (handle (\(_ :: IOException) -> return ()) . killThread) [threadId, tiClose]
    boardState <- readTVarIO vBoardState
    when (isGameUser move) $ saveAsPGN boardState
    unless (isJust $ Api.gameResult boardState) $ case relation move of
      MyMove -> hPutStrLn h "5 resign"
      OponentsMove -> hPutStrLn h "5 resign"
      Observing -> hPutStrLn h $ "5 unobserve " ++ show (gameId move)
      _ -> return ()


resizeFrame :: Frame () -> TVar Api.BoardState -> Panel() -> IO ()
resizeFrame f vBoardState p_board = do
  (Size w h) <- windowGetClientSize f
  Api.resize p_board vBoardState
  let x = max w (h-66)
  windowSetClientSize f $ Size x (x+66)
  void $ windowLayout f


updateBoardLayout :: Panel() -> Panel() -> Panel() -> Panel() -> TVar Api.BoardState -> IO ()
updateBoardLayout pback board white black vBoardState = do
  state <- readTVarIO vBoardState
  set pback [ layout := column 0 [ hfill $ widget (if Api.perspective state == White then black else white)
                                 , stretch $ minsize (Size 320 320) $ shaped $ widget board
                                 , hfill $ widget (if Api.perspective state == White then white else black)]]


wxPieceSetsMenu :: Menu () -> TVar Api.BoardState -> Panel () -> IO ()
wxPieceSetsMenu ctxMenu vState p = do
  sub <- menuPane [text := "Piece Sets"]
  mapM_ (\ps -> menuItem sub [ text := display ps, on command := Api.setPieceSet vState ps >> repaint p ]) pieceSets
  void $ menuSub ctxMenu sub [ text := "Piece Sets" ]


onKeyUpHandler :: TVar Api.BoardState -> Handle -> StatusField -> EventKey -> IO ()
onKeyUpHandler vBoardState h sf evt
  | (keyKey evt == KeyControl) && isNoneDown (keyModifiers evt) =
      Api.promotion `fmap` readTVarIO vBoardState >>= \p -> do
        hPutStrLn h $ "5 promote " ++ show p
        set sf [text := "=" ]
  | otherwise = return ()

