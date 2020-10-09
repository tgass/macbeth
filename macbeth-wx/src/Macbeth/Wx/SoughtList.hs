module Macbeth.Wx.SoughtList (
  wxSoughtList
) where


import           Control.Concurrent.STM
import           Control.Monad
import           Data.List (elemIndex)
import           Graphics.UI.WX hiding (when)
import           Graphics.UI.WXCore hiding (when)
import           Macbeth.Fics.Api.Seek
import           Macbeth.Fics.Api.OngoingGame hiding (gameType, isRated)
import           Macbeth.Fics.Message
import qualified Macbeth.Fics.Commands as Cmds
import qualified Macbeth.Wx.RuntimeEnv as E
import           Macbeth.Wx.Utils

data SoughtOpts = SoughtOpts { computerOffers :: MenuItem ()
                             , unregisteredPlayers :: MenuItem ()
                             , unratedOffers :: MenuItem ()
                             , ratedOffers :: MenuItem () }

wxSoughtList :: Panel () -> E.RuntimeEnv -> IO (ListCtrl (), Message -> IO ())
wxSoughtList slp env = do
    sl  <- listCtrlEx slp (wxLC_REPORT .+. wxLC_SORT_ASCENDING)
                                    [columns := [ ("#", AlignLeft, 100)
                                    , ("Handle", AlignLeft, 100)
                                    , ("Rating", AlignLeft, 100)
                                    , ("Time (start inc.)", AlignRight, 100)
                                    , ("Type", AlignRight, 100)]
                                    ]
    set sl [on listEvent := onSeekListEvent sl env]

    ctxMenu <- menuPane []
    soughtOpts <- getSoughtOpts ctxMenu
    vSoughtList <- newTVarIO ([] :: [Seek])

    set (computerOffers soughtOpts) [on command := filterSoughtList sl soughtOpts vSoughtList]
    set (unregisteredPlayers soughtOpts) [on command := filterSoughtList sl soughtOpts vSoughtList]
    set (unratedOffers soughtOpts) [on command := filterSoughtList sl soughtOpts vSoughtList]
    set (ratedOffers soughtOpts) [on command := filterSoughtList sl soughtOpts vSoughtList]

    listItemRightClickEvent sl (\evt -> do
                pt' <- listEventGetPoint evt
                menuPopup ctxMenu pt' sl)

    flip (listCtrlAssignImageList sl) wxIMAGE_LIST_SMALL =<< images

    let handler cmd = case cmd of
            NewSeek seek -> do
              atomically $ modifyTVar vSoughtList (++ [seek])
              showSeek' <- showSeek soughtOpts
              when (showSeek' seek) $ addSeek sl seek

            ClearSeek -> itemsDelete sl

            RemoveSeeks gameIds -> sequence_ $ fmap (deleteSeek sl) gameIds

            _ -> return ()

    return (sl, handler)


images :: IO (ImageList ())
images = imageListFromFiles (sz 16 16) $ fmap E.getIconFilePath ["fa-user", "fa-desktop"]


addSeek :: ListCtrl () -> Seek -> IO ()
addSeek l seek = do
  count <- listCtrlGetItemCount l

  item' <- listItemCreate
  listItemSetId item' count
  --when (Computer `elem` titles seek) $ listItemSetBackgroundColour item (colorRGB 125 149 184)
  listItemSetImage item' $ if Computer `elem` titles seek then 1 else 0

  _ <- listCtrlInsertItem l item'
  mapM_ (\(col, txt) -> listCtrlSetItem l count col txt (-1)) (zip [0..] (toList seek))


showSeek :: SoughtOpts -> IO (Seek -> Bool)
showSeek opts = do
  computerOffers' <- get (computerOffers opts) checked
  unregisteredPlayers' <- get (unregisteredPlayers opts) checked
  unratedOffers' <- get (unratedOffers opts) checked
  ratedOffers' <- get (ratedOffers opts) checked
  return $ \s -> (gameType s `elem` [Untimed, Standard, Blitz, Lightning]) &&
                 ((Computer `elem` titles s) .>. computerOffers') &&
                 ((Unregistered `elem` titles s) .>. unregisteredPlayers') &&
                 (isRated s .>. ratedOffers') &&
                 (not (isRated s) .>. unratedOffers')


(.>.) :: Bool -> Bool -> Bool
(.>.) p q = (not p) || q


filterSoughtList :: ListCtrl () -> SoughtOpts -> TVar [Seek] -> IO ()
filterSoughtList sl opts vSoughtList = do
  showSeek' <- showSeek opts
  soughts <- readTVarIO vSoughtList
  void $ listCtrlDeleteAllItems sl
  mapM_ (\s -> when (showSeek' s) $ addSeek sl s) soughts


onSeekListEvent :: ListCtrl() -> E.RuntimeEnv -> EventList -> IO ()
onSeekListEvent sl env evt = case evt of
  ListItemActivated idx -> listCtrlGetItemText sl idx >>= Cmds.play env
  _ -> return ()


deleteSeek :: ListCtrl () -> Int -> IO ()
deleteSeek sl gameId'' = do
  seeks <- get sl items
  case elemIndex gameId'' $ fmap (read . (!! 0)) seeks of
    Just idx' -> itemDelete sl idx'
    Nothing -> return ()


getSoughtOpts :: Menu () -> IO SoughtOpts
getSoughtOpts ctxMenu = SoughtOpts
  <$> menuItem ctxMenu [ text := "Show computer offers", checkable := True, checked := True]
  <*> menuItem ctxMenu [ text := "Show unregistered players", checkable := True, checked := True]
  <*> menuItem ctxMenu [ text := "Show unrated offers", checkable := True, checked := True]
  <*> menuItem ctxMenu [ text := "Show rated offers", checkable := True, checked := True]

toList :: Seek -> [String]
toList (Seek gameId'' name' _ rating' time inc isRated' gameType' _ _) =
  [show gameId'', name', show rating', show time ++ " " ++ show inc, show gameType' ++ " " ++ showIsRated isRated']
  where showIsRated True = "rated"
        showIsRated False = "unrated"
