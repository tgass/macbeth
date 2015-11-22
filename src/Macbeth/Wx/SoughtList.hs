module Macbeth.Wx.SoughtList (
  wxSoughtList
) where

import Macbeth.Api.Seek
import Macbeth.Api.Game hiding (gameType, isRated)
import Macbeth.Api.CommandMsg
import Macbeth.Wx.Utils

import Control.Concurrent.STM
import Control.Applicative
import Control.Monad
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import System.IO
import Data.List (elemIndex)

data SoughtOpts = SoughtOpts { computerOffers :: MenuItem ()
                             , unregisteredPlayers :: MenuItem ()
                             , unratedOffers :: MenuItem ()
                             , ratedOffers :: MenuItem () }

wxSoughtList :: Panel () -> Handle -> IO (ListCtrl (), CommandMsg -> IO ())
wxSoughtList slp h = do
    sl  <- listCtrl slp [columns := [ ("#", AlignLeft, -1)
                                    , ("handle", AlignLeft, -1)
                                    , ("rating", AlignLeft, -1)
                                    , ("Time (start inc.)", AlignRight, -1)
                                    , ("type", AlignRight, -1)]
                                    ]
    set sl [on listEvent := onSeekListEvent sl h]
    listCtrlSetColumnWidths sl 100

    ctxMenu <- menuPane []
    soughtOpts <- getSoughtOpts ctxMenu
    vSoughtList <- newTVarIO ([] :: [Seek])

    set (computerOffers soughtOpts) [on command := filterSoughtList sl soughtOpts vSoughtList]
    set (unregisteredPlayers soughtOpts) [on command := filterSoughtList sl soughtOpts vSoughtList]
    set (unratedOffers soughtOpts) [on command := filterSoughtList sl soughtOpts vSoughtList]
    set (ratedOffers soughtOpts) [on command := filterSoughtList sl soughtOpts vSoughtList]

    listItemRightClickEvent sl (\evt -> do
                pt <- listEventGetPoint evt
                menuPopup ctxMenu pt sl)

    imagePaths <- mapM getAbsoluteFilePath imageFiles  -- make relative to application
    images     <- imageListFromFiles (sz 16 16) imagePaths

    listCtrlAssignImageList sl images wxIMAGE_LIST_SMALL

    let handler cmd = case cmd of
            NewSeek seek -> do
              atomically $ modifyTVar vSoughtList (\sl -> sl ++ [seek])
              showSeek' <- showSeek soughtOpts
              when (showSeek' seek) $ addSeek sl seek

            ClearSeek -> itemsDelete sl

            RemoveSeeks gameIds -> do
              seeks <- get sl items
              sequence_ $ fmap (deleteSeek sl . findSeekIdx seeks) gameIds

            _ -> return ()

    return (sl, handler)

imageNames = ["fa-user", "fa-desktop"]


imageFiles = map (\name -> "resources/icons/" ++ name ++ ".png") imageNames


addSeek :: ListCtrl () -> Seek -> IO ()
addSeek l seek = do
  count <- listCtrlGetItemCount l

  item <- listItemCreate
  listItemSetId item count
  --when (Computer `elem` titles seek) $ listItemSetBackgroundColour item (colorRGB 125 149 184)
  listItemSetImage item $ if Computer `elem` titles seek then 1 else 0

  listCtrlInsertItem l item
  mapM_ (\(column,txt) -> listCtrlSetItem l count column txt (-1)) (zip [0..] (toList seek))


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
  listCtrlDeleteAllItems sl
  mapM_ (\s -> when (showSeek' s) $ addSeek sl s) soughts


onSeekListEvent :: ListCtrl() -> Handle -> EventList -> IO ()
onSeekListEvent sl h eventList = case eventList of
  ListItemActivated idx -> do
    seeks <- get sl items
    hPutStrLn h $ "4 play " ++ show (read $ head (seeks !! idx) :: Int)
  _ -> return ()


findSeekIdx :: [[String]] -> Int -> Maybe Int
findSeekIdx seeks gameId = elemIndex gameId $ fmap (read . (!! 0)) seeks


deleteSeek :: ListCtrl () -> Maybe Int -> IO ()
deleteSeek sl (Just id) = itemDelete sl id
deleteSeek _ _ = return ()


getSoughtOpts :: Menu () -> IO SoughtOpts
getSoughtOpts ctxMenu = SoughtOpts
  <$> menuItem ctxMenu [ text := "Show computer offers", checkable := True, checked := True]
  <*> menuItem ctxMenu [ text := "Show unregistered players", checkable := True, checked := True]
  <*> menuItem ctxMenu [ text := "Show unrated offers", checkable := True, checked := True]
  <*> menuItem ctxMenu [ text := "Show rated offers", checkable := True, checked := True]

toList :: Seek -> [String]
toList (Seek id name _ rating time inc isRated gameType _ _) =
  [show id, name, show rating, show time ++ " " ++ show inc, show gameType ++ " " ++ showIsRated isRated]
  where showIsRated True = "rated"
        showIsRated False = "unrated"
