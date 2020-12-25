module Macbeth.Wx.PlayersList (
  wxPlayersList
) where

import           Control.Applicative
import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Graphics.UI.WX hiding (when)
import           Graphics.UI.WXCore hiding (when)
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Player
import           Macbeth.Fics.Message
import qualified Macbeth.Fics.Commands as Cmds
import           Macbeth.Wx.Icons (Icon(..))
import qualified Macbeth.Wx.Icons as Icons
import qualified Macbeth.Wx.RuntimeEnv as E
import           Macbeth.Wx.Utils


data CtxMenu = CtxMenu {
    match :: MenuItem ()
  , finger :: MenuItem ()
  , history :: MenuItem ()
  , observe :: MenuItem ()
  , partner :: MenuItem ()
  , chat :: MenuItem ()
  , _sortBy :: MenuItem ()
}

data CtxSortMenu = CtxSortMenu {
    sortByName :: MenuItem()
  , sortByStatus :: MenuItem()
  , sortByRating :: MenuItem()
}


wxPlayersList :: Panel () -> E.RuntimeEnv -> Chan Message -> IO (ListCtrl (), Message -> IO ())
wxPlayersList slp env chan = do
    players <-newTVarIO ([] :: [Player])
    sl <- listCtrl slp [columns := [
        ("Handle", AlignLeft, -1)
      , ("State", AlignRight, -1)
      , ("Rating", AlignRight, -1)
      , ("Title", AlignRight, -1)]]

    listCtrlSetColumnWidths sl 120
    flip (listCtrlAssignImageList sl) wxIMAGE_LIST_SMALL =<< icons

    ctxMenuPane <- menuPane []
    ctxSortMenuPane <- menuPane []

    ctxSortMenu' <- ctxSortMenu ctxSortMenuPane
    ctxMenu' <- ctxMenu ctxMenuPane ctxSortMenuPane

    listItemRightClickEvent sl (\evt -> do
      player <- listEventGetIndex evt >>= get sl . item
      set (match ctxMenu') [on command := Cmds.match env $ head player]
      set (finger ctxMenu') [on command := Cmds.finger env $ Just $ head player]
      set (history ctxMenu') [on command := Cmds.history env $ Just $ head player]
      set (observe ctxMenu') [on command := Cmds.observe env $ head player]
      set (partner ctxMenu') [on command := Cmds.partner env $ head player]
      set (chat ctxMenu') [on command := writeChan chan $ WxChat (UserChat $ head player) ]
      listEventGetPoint evt >>= flip (menuPopup ctxMenuPane) sl)

    set slp [ on (menu $ sortByName ctxSortMenu') := sortPlayers sl ctxSortMenu' players ]
    set slp [ on (menu $ sortByStatus ctxSortMenu') := sortPlayers sl ctxSortMenu' players ]
    set slp [ on (menu $ sortByRating ctxSortMenu') := sortPlayers sl ctxSortMenu' players ]

    return (sl, handler sl ctxSortMenu' players)


handler :: ListCtrl () -> CtxSortMenu -> TVar [Player] -> Message -> IO ()
handler sl sortMenu players = \case
  Players players' -> do
    atomically $ modifyTVar players (const players')
    sortPlayers sl sortMenu players

  _ -> return ()


sortPlayers :: ListCtrl () -> CtxSortMenu -> TVar [Player] -> IO ()
sortPlayers sl sortMenu players = do
  sortOrder' <- sortFoo sortMenu
  players' <- readTVarIO players
  _ <- listCtrlDeleteAllItems sl
  sequence_ $ fmap (addPlayer sl) (sortBy sortOrder' players')


sortFoo :: CtxSortMenu -> IO (Player -> Player -> Ordering)
sortFoo ctxMenu' = do
  name' <- foo (fmap toLower . name . handle) (sortByName ctxMenu')
  status' <- foo status (sortByStatus ctxMenu')
  rating' <- foo (Down . rating) (sortByRating ctxMenu')

  return $ fromMaybe (comparing (name . handle)) (name' <|> status' <|> rating')


foo :: Ord a => (b -> a) -> MenuItem () -> IO (Maybe (b -> b -> Ordering))
foo f mu = flip whenMaybe (comparing f) <$> get mu checked


whenMaybe :: Bool -> a -> Maybe a
whenMaybe x = (guard x >>) . Just


icons :: IO (ImageList ())
icons = imageListFromFiles (sz 16 16) $ fmap Icons.filepath [UserIcon, DesktopIcon]


addPlayer :: ListCtrl () -> Player -> IO ()
addPlayer l player = do
  count <- listCtrlGetItemCount l

  item' <- listItemCreate
  listItemSetId item' count
--  when (Computer `elem` handleType (handle player)) $ listItemSetBackgroundColour item (colorRGB (255 :: Int) 255 188)
  listItemSetImage item' (fmap imageIdx handleType $ handle player)

  _ <- listCtrlInsertItem l item'
  mapM_ (\(col, txt) -> listCtrlSetItem l count col txt (-1)) (zip [0..] (toList player))

  where
    imageIdx :: [HandleType] -> Int
    imageIdx types = if Computer `elem` types then 1 else 0


ctxMenu :: Menu () -> Menu () -> IO CtxMenu
ctxMenu ctxMenu' sub = CtxMenu
  <$> menuItem ctxMenu' [ text := "Match"]
  <*> menuItem ctxMenu' [ text := "Finger"]
  <*> menuItem ctxMenu' [ text := "History"]
  <*> menuItem ctxMenu' [ text := "Observe"]
  <*> menuItem ctxMenu' [ text := "Partner"]
  <*> menuItem ctxMenu' [ text := "Chat"]
  <*> menuSub ctxMenu' sub [ text := "Sort by" ]


ctxSortMenu :: Menu () -> IO CtxSortMenu
ctxSortMenu menu' = CtxSortMenu
  <$> menuRadioItem menu' [ text := "Name"]
  <*> menuRadioItem menu' [ text := "State"]
  <*> menuRadioItem menu' [ text := "Rating"]


toList :: Player -> [String]
toList (Player rating' status' (UserHandle username ht)) =
  [username, toStringStatus status', show rating', showHandleType ht]
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

