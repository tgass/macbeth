module Macbeth.Wx.PlayersList (
  wxPlayersList
) where

import Macbeth.Fics.Api.Player
import Macbeth.Fics.FicsMessage
import Macbeth.Wx.Utils

import Paths

import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import qualified System.IO as IO


data CtxMenu = CtxMenu {
    match :: MenuItem ()
  , finger :: MenuItem ()
  , history :: MenuItem ()
  , observe :: MenuItem ()
  , partner :: MenuItem ()
}

wxPlayersList :: Panel () -> IO.Handle -> IO (ListCtrl (), FicsMessage -> IO ())
wxPlayersList slp h = do
    sl <- listCtrl slp [columns := [
        ("Handle", AlignLeft, -1)
      , ("State", AlignRight, -1)
      , ("Rating", AlignRight, -1)
      , ("Title", AlignRight, -1)]]

    listCtrlSetColumnWidths sl 120
    images >>= flip (listCtrlAssignImageList sl) wxIMAGE_LIST_SMALL

    ctxMenu <- menuPane []
    ctxMenuPopup <- createCtxMenu ctxMenu

    listItemRightClickEvent sl (\evt -> do
      player <- listEventGetIndex evt >>= get sl . item

      set (finger ctxMenuPopup) [on command := IO.hPutStrLn h $ "6 finger " ++ head player]
      set (partner ctxMenuPopup) [on command := IO.hPutStrLn h $ "6 partner " ++ head player]
      set (match ctxMenuPopup) [on command := IO.hPutStrLn h $ "6 match " ++ head player]
      set (history ctxMenuPopup) [on command := IO.hPutStrLn h $ "6 history " ++ head player]
      set (observe ctxMenuPopup) [on command := IO.hPutStrLn h $ "6 observe " ++ head player]

      pt <- listEventGetPoint evt
      menuPopup ctxMenu pt sl)

    return (sl, handler sl)


handler :: ListCtrl () -> FicsMessage -> IO ()
handler sl cmd = case cmd of
  Players players -> do
    listCtrlDeleteAllItems sl
    sequence_ $ fmap (addPlayer sl) players
  _ -> return ()


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
  --when (Computer `elem` titles seek) $ listItemSetBackgroundColour item (colorRGB 125 149 184)
  listItemSetImage item (fmap imageIdx handleType $ handle player)

  listCtrlInsertItem l item
  mapM_ (\(column,txt) -> listCtrlSetItem l count column txt (-1)) (zip [0..] (toList player))

  where
    imageIdx :: HandleType -> Int
    imageIdx Computer = 1
    imageIdx _ = 0


createCtxMenu :: Menu () -> IO CtxMenu
createCtxMenu ctxMenu = CtxMenu
  <$> menuItem ctxMenu [ text := "Match"]
  <*> menuItem ctxMenu [ text := "Finger"]
  <*> menuItem ctxMenu [ text := "History"]
  <*> menuItem ctxMenu [ text := "Observe"]
  <*> menuItem ctxMenu [ text := "Partner"]


toList :: Player -> [String]
toList (Player rating status (Handle username ht)) =
  [username, toStringStatus status, show rating, showHandleType ht]
  where
    toStringStatus InvolvedInAGame = "Playing"
    toStringStatus RunningASimulMatch = "Playing Simul"
    toStringStatus NotOpenForMatch = "Not Open"
    toStringStatus ExaminingAGame = "Examining"
    toStringStatus InactiveOrBusy = "Inactive"
    toStringStatus NotBusy = "Not Busy"
    toStringStatus InvolvedInATournament = "Tournament"

    showHandleType ht
      | ht `elem` [None, Unregistered, Computer] = ""
      | otherwise = show ht

