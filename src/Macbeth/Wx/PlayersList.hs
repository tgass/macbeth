module Macbeth.Wx.PlayersList (
  wxPlayersList
) where

import Macbeth.Fics.Api.Player
import Macbeth.Fics.FicsMessage
import Macbeth.Wx.Utils

import Paths

import Data.List
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import System.IO


data CtxMenu = CtxMenu {
    match :: MenuItem ()
  , finger :: MenuItem ()
  , history :: MenuItem ()
  , observe :: MenuItem ()
  , partner :: MenuItem ()
  , follow :: MenuItem ()
}

wxPlayersList :: Panel () -> Handle -> IO (ListCtrl (), FicsMessage -> IO ())
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
    let updateM user (f, cmd) = set (f ctxMenuPopup) [on command := hPutStrLn h $ "6 " ++ cmd ++ " " ++ user]

    listItemRightClickEvent sl (\evt -> do
      player <- listEventGetIndex evt >>= get sl . item
      sequence_ $ fmap (updateM (head player)) [(finger, "finger"), (partner, "partner"),
        (match, "match"), (history, "history"), (observe, "observe"), (follow, "follow")]
      listEventGetPoint evt >>= flip (menuPopup ctxMenu) sl)

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
    imageIdx :: [HandleType] -> Int
    imageIdx types = if Computer `elem` types then 1 else 0


createCtxMenu :: Menu () -> IO CtxMenu
createCtxMenu ctxMenu = CtxMenu
  <$> menuItem ctxMenu [ text := "Match"]
  <*> menuItem ctxMenu [ text := "Finger"]
  <*> menuItem ctxMenu [ text := "History"]
  <*> menuItem ctxMenu [ text := "Observe"]
  <*> menuItem ctxMenu [ text := "Partner"]
  <*> menuItem ctxMenu [ text := "Follow"]

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

