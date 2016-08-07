{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.PlayersList (
  wxPlayersList
) where

import Macbeth.Fics.Api.Player
import Macbeth.Fics.Api.Chat
import Macbeth.Fics.FicsMessage
import Macbeth.Wx.Utils

import Paths

import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Ord
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import System.IO


data CtxMenu = CtxMenu {
    match :: MenuItem ()
  , finger :: MenuItem ()
  , history :: MenuItem ()
  , observe :: MenuItem ()
  , partner :: MenuItem ()
  , chat :: MenuItem()
  , sortByName :: MenuItem()
  , sortByStatus :: MenuItem()
  , sortByRating :: MenuItem()
}

data SortOrder = Name | Status | Rating deriving Show

data ListCfg = ListCfg {
    sortOrder :: TVar SortOrder
  , players :: TVar [Player]
}

wxPlayersList :: Panel () -> Handle -> Chan FicsMessage -> IO (ListCtrl (), FicsMessage -> IO ())
wxPlayersList slp h chan = do
    cfg <- ListCfg <$> newTVarIO Name <*> newTVarIO ([] :: [Player])
    sl <- listCtrl slp [columns := [
        ("Handle", AlignLeft, -1)
      , ("State", AlignRight, -1)
      , ("Rating", AlignRight, -1)
      , ("Title", AlignRight, -1)]]

    listCtrlSetColumnWidths sl 120
    images >>= flip (listCtrlAssignImageList sl) wxIMAGE_LIST_SMALL

    ctxMenu <- menuPane []
    ctxMenuPopup <- createCtxMenu ctxMenu
    set (sortByName ctxMenuPopup) [ on command := atomically (writeTVar (sortOrder cfg) Name) >> sortPlayers sl cfg
                                  , checked := True ]
    set (sortByStatus ctxMenuPopup) [ on command := atomically (writeTVar (sortOrder cfg) Status) >> sortPlayers sl cfg ]
    set (sortByRating ctxMenuPopup) [ on command := atomically (writeTVar (sortOrder cfg) Rating) >> sortPlayers sl cfg ]

    listItemRightClickEvent sl (\evt -> do
      player <- listEventGetIndex evt >>= get sl . item
      set (match ctxMenuPopup) [on command := hPutStrLn h $ "6 match " ++ head player]
      set (finger ctxMenuPopup) [on command := hPutStrLn h $ "6 finger " ++ head player]
      set (history ctxMenuPopup) [on command := hPutStrLn h $ "6 history " ++ head player]
      set (observe ctxMenuPopup) [on command := hPutStrLn h $ "6 observe " ++ head player]
      set (partner ctxMenuPopup) [on command := hPutStrLn h $ "6 partner " ++ head player]
      set (chat ctxMenuPopup) [on command := writeChan chan $ Chat $ OpenChat (head player) Nothing ]
      listEventGetPoint evt >>= flip (menuPopup ctxMenu) sl)

    return (sl, handler sl cfg)


handler :: ListCtrl () -> ListCfg -> FicsMessage -> IO ()
handler sl cfg = \case
  Players players' -> do
    atomically $ modifyTVar (players cfg) (const players')
    sortPlayers sl cfg

  _ -> return ()


sortPlayers :: ListCtrl () -> ListCfg -> IO ()
sortPlayers sl cfg = do
  sortOrder' <- readTVarIO (sortOrder cfg)
  players' <- readTVarIO (players cfg)
  listCtrlDeleteAllItems sl
  sequence_ $ fmap (addPlayer sl) (sortBy (convert sortOrder') players')
  where
    convert :: SortOrder -> (Player -> Player -> Ordering)
    convert Name = comparing handle
    convert Status = comparing status
    convert Rating = comparing rating


images :: IO (ImageList ())
images = do
  let imageFiles = map (\name -> "icons/" ++ name ++ ".gif") ["fa-user", "fa-desktop"]
  imagePaths <- mapM getDataFileName imageFiles
  imageListFromFiles (sz 16 16) imagePaths


addPlayer :: ListCtrl () -> Player -> IO ()
addPlayer l player = do
  count <- listCtrlGetItemCount l

  item <- listItemCreate
  listItemSetId item count
  when (Computer `elem` handleType (handle player)) $ listItemSetBackgroundColour item (colorRGB 255 255 188)
  listItemSetImage item (fmap imageIdx handleType $ handle player)

  listCtrlInsertItem l item
  mapM_ (\(column,txt) -> listCtrlSetItem l count column txt (-1)) (zip [0..] (toList player))

  where
    imageIdx :: [HandleType] -> Int
    imageIdx types = if Computer `elem` types then 1 else 0


createCtxMenu :: Menu () -> IO CtxMenu
createCtxMenu ctxMenu = CtxMenu
  <$> menuItem ctxMenu [ text := "Match"]
  <*> menuItem ctxMenu [ text := "Finger"]
  <*> menuItem ctxMenu [ text := "History"]
  <*> menuItem ctxMenu [ text := "Observe"]
  <*> menuItem ctxMenu [ text := "Partner"]
  <*> menuItem ctxMenu [ text := "Chat"]
  <*> (menuLine ctxMenu >>
      menuRadioItem ctxMenu [ text := "Sort by Name"])
  <*> menuRadioItem ctxMenu [ text := "Sort by State"]
  <*> menuRadioItem ctxMenu [ text := "Sort by Rating"]


toList :: Player -> [String]
toList (Player rating status (UserHandle username ht)) =
  [username, toStringStatus status, show rating, showHandleType ht]
  where
    toStringStatus InvolvedInAGame = "Playing"
    toStringStatus RunningASimulMatch = "Playing Simul"
    toStringStatus NotOpenForMatch = "Not Open"
    toStringStatus ExaminingAGame = "Examining"
    toStringStatus InactiveOrBusy = "Inactive"
    toStringStatus NotBusy = "Not Busy"
    toStringStatus InvolvedInATournament = "Tournament"

    showHandleType = intercalate ", " . fmap show .
      filter (not . flip elem [Unregistered, Computer, NOT_DOCUMENTED, ServiceRepresentative])

