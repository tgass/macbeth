{-# LANGUAGE LambdaCase, ScopedTypeVariables, MultiWayIf #-}

module Macbeth.Wx.Game.Game (
  wxGame
) where

import Macbeth.Fics.FicsMessage hiding (gameId)
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Chat
import Macbeth.Utils.PGN
import Macbeth.Wx.Game.PieceSet
import Macbeth.Wx.Game.StatusPanel
import Macbeth.Wx.Game.GameSounds
import qualified Macbeth.Fics.Api.Move as M
import qualified Macbeth.Fics.Api.Game as G
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
import Graphics.UI.WX hiding (when, position, play, point, white, black)
import Graphics.UI.WXCore hiding (when, Timer, black, white, point)
import System.IO

eventId :: Int
eventId = wxID_HIGHEST + 1

wxGame :: E.RuntimeEnv -> GameId -> G.GameParams  -> Chan FicsMessage -> IO ()
wxGame env gameId gameParams' chan = do
  let h = E.handle env
  username <- E.username env
  vCmd <- newEmptyMVar

  f <- frame [ text := G.toTitle gameId gameParams']
  p_back <- panel f []

  -- board
  let boardState = Api.initBoardState gameId gameParams' username
  vBoardState <- newTVarIO boardState
  p_board <- panel p_back [ on paint := Board.draw vBoardState]

  -- player panels
  (p_white, updateClockW) <- createStatusPanel p_back White vBoardState env
  (p_black, updateClockB) <- createStatusPanel p_back Black vBoardState env

  -- layout helper
  let updateBoardLayoutIO = updateBoardLayout p_back p_board p_white p_black vBoardState >> refit f
  updateBoardLayoutIO


  status <- statusField []
  promotion <- statusField [ statusWidth := 30, text := "=Q"]

  -- context menu
  ctxMenu <- menuPane []
  _ <- menuItem ctxMenu [ text := "Turn board", on command :=
    Api.invertPerspective vBoardState >> updateBoardLayoutIO >> repaint p_board >> resizeFrame f vBoardState p_board]
  when (G.isGameUser' gameParams') $ do
     menuLine ctxMenu
     _ <- menuItem ctxMenu [ text := "Request takeback 1", on command := hPutStrLn h "4 takeback 1"]
     _ <- menuItem ctxMenu [ text := "Request takeback 2", on command := hPutStrLn h "4 takeback 2"]
     _ <- menuItem ctxMenu [ text := "Request abort", on command := hPutStrLn h "4 abort"]
     _ <- menuItem ctxMenu [ text := "Offer draw", on command := hPutStrLn h "4 draw" ]
     _ <- menuItem ctxMenu [ text := "Resign", on command := hPutStrLn h "4 resign" ]
     menuLine ctxMenu
     _ <- menuItem ctxMenu [ text := "Chat", on command := writeChan chan $ Chat $ OpenChat (fromMaybe "" $ G.nameOponent username gameParams') (Just gameId)]

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
    | Utl.onlyKey evt 'T' && G.isGameUser' gameParams' -> writeChan chan $ Chat $ OpenChat (fromMaybe "" $ G.nameOponent username gameParams') (Just gameId)
    | (keyKey evt == KeyEscape) && isNoneDown (keyModifiers evt) -> Api.discardDraggedPiece vBoardState >> repaint p_board

    | Utl.onlyKey evt 'N' -> hPutStrLn h "5 decline"
    | Utl.onlyKey evt 'Y' -> hPutStrLn h "5 accept"

    | Utl.keyWithMod evt 'W' justControl -> close f
    | Utl.keyWithMod evt 'O' justControl -> Api.togglePromotion vBoardState >>= \p -> set promotion [text := "=" ++ show p]
    | otherwise -> return ())

  windowOnKeyUp p_board $ onKeyUpHandler vBoardState h promotion

  --set layout
  set f [ statusBar := [status] ++ [promotion | Api.isGameUser boardState]
        , layout := fill $ widget p_back
        , on resize := resizeFrame f vBoardState p_board]

  -- necessary: after GameResult no more events are handled
  tiClose <- dupChan chan >>= Utl.registerWxCloseEventListenerWithThreadId f

  threadId <- forkIO $ Utl.eventLoop eventId chan vCmd f
  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd ->
    updateClockW cmd >> updateClockB cmd >> gameSounds env boardState cmd >> case cmd of

    GameMove ctx move' -> when (M.gameId move' == gameId) $ do
      set status [text := show ctx]
      Api.update vBoardState move' ctx
      when (M.isNextMoveUser move') $ Api.performPreMoves vBoardState h
      repaint p_board

    GameResult result -> when (R.gameId result == gameId) $ do
      set status [text := R.toString result]
      Api.setResult vBoardState (R.result result)
      repaint p_board
      when (Api.isGameUser boardState) $ readTVarIO vBoardState >>= saveAsPGN
      hPutStrLn h "4 iset seekinfo 1"
      killThread threadId

    DrawRequest user -> set status [text := user ++ " offered a draw. Accept? (y/n)"]

    AbortRequest user -> set status [text := user ++ " would like to abort the game. Accept? (y/n)"]

    TakebackRequest user numTakeback -> set status [text := user ++ " would like to take back " ++ show numTakeback ++ " half move(s). Accept? (y/n)"]

    OponentDecline user sub
      | sub `elem` [DrawReq, TakebackReq, AbortReq] -> set status [text := user ++ " declines the " ++ show sub ++ " request."]
      | otherwise -> return ()

    PromotionPiece p -> Api.setPromotion p vBoardState >> set promotion [text := "=" ++ show p]

    _ -> return ()

  windowOnDestroy f $ do
    sequence_ $ fmap (handle (\(_ :: IOException) -> return ()) . killThread) [threadId, tiClose]
    boardState' <- readTVarIO vBoardState

    when (isNothing (Api.gameResult boardState') && not (Api.isGameUser boardState)) $
      hPutStrLn h $ "5 unobserve " ++ show gameId


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

