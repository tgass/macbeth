module Macbeth.Wx.GamesList (
  wxGamesList
) where

import Macbeth.Fics.Api.Game
import Macbeth.Fics.FicsMessage hiding (gameId)
import Macbeth.Wx.Utils

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import Graphics.UI.WX hiding (refresh)
import Graphics.UI.WXCore
import System.IO

data CtxMenu = CtxMenu {
    refresh :: MenuItem ()
  , showRated :: MenuItem ()
  , showUnrated :: MenuItem ()
  , sortByGameId :: MenuItem ()
  , sortByWhitesName :: MenuItem ()
  , sortByWhitesRating :: MenuItem ()
  , sortByBlacksName :: MenuItem ()
  , sortByBlacksRating :: MenuItem ()
  , sortByGameType :: MenuItem ()
}

data ListCfg = ListCfg {
  games :: TVar [Game]
}

wxGamesList :: Panel () -> Handle -> IO (ListCtrl (), FicsMessage -> IO ())
wxGamesList glp h = do
  cfg <- ListCfg <$> newTVarIO ([] :: [Game])
  gl  <- listCtrl glp [ columns :=
      [ ("#", AlignLeft, -1)
      , ("Player 1", AlignLeft, -1)
      , ("Rating", AlignLeft, -1)
      , ("Player 2", AlignLeft, -1)
      , ("Rating", AlignLeft, -1)
      , ("Game type", AlignLeft, -1)
      ]]
  listCtrlSetColumnWidths gl 100
  set gl [on listEvent := onGamesListEvent gl h]

  glCtxMenu <- menuPane []
  ctxMenuPopup <- getGamesOpts glCtxMenu

  listItemRightClickEvent gl $ \evt -> do
    pt <- listEventGetPoint evt
    menuPopup glCtxMenu pt gl

  set (refresh ctxMenuPopup) [on command := hPutStrLn h "4 games"]
  set (sortByGameId ctxMenuPopup) [ on command := displayGames gl ctxMenuPopup cfg]
  set (sortByWhitesName ctxMenuPopup) [ on command := displayGames gl ctxMenuPopup cfg]
  set (sortByWhitesRating ctxMenuPopup) [ on command := displayGames gl ctxMenuPopup cfg]
  set (sortByBlacksName ctxMenuPopup) [ on command := displayGames gl ctxMenuPopup cfg]
  set (sortByBlacksRating ctxMenuPopup) [ on command :=displayGames gl ctxMenuPopup cfg]
  set (sortByGameType ctxMenuPopup) [ on command := displayGames gl ctxMenuPopup cfg]
  set (showRated ctxMenuPopup) [on command := displayGames gl ctxMenuPopup cfg]
  set (showUnrated ctxMenuPopup) [on command := displayGames gl ctxMenuPopup cfg]

  return (gl, handler gl ctxMenuPopup cfg)


handler :: ListCtrl () -> CtxMenu -> ListCfg -> FicsMessage -> IO ()
handler gl ctx cfg cmd = case cmd of
  Games games' -> do
    atomically $ modifyTVar (games cfg) (const games')
    displayGames gl ctx cfg

  _ -> return ()


onGamesListEvent :: ListCtrl() -> Handle -> EventList -> IO ()
onGamesListEvent gl h evt = case evt of
  ListItemActivated idx -> listCtrlGetItemText gl idx >>= hPutStrLn h . ("4 observe " ++)
  _ -> return ()


displayGames :: ListCtrl () -> CtxMenu -> ListCfg -> IO ()
displayGames gl opts cfg = do
  sortOrder <- sortFoo opts
  games' <- sortBy sortOrder <$> readTVarIO (games cfg)

  showRated' <- get (showRated opts) checked
  showUnrated' <- get (showUnrated opts) checked
  set gl [items := [ toList g | g <- games'
                   , (isRated (settings g) == showRated') ||
                     (isRated (settings g) == not showUnrated')
                   , not $ isPrivate $ settings g]]


sortFoo :: CtxMenu -> IO (Game -> Game -> Ordering)
sortFoo ctxMenu = do
  gameId' <- flip whenMaybe (comparing gameId) <$> get (sortByGameId ctxMenu) checked
  nameW' <- flip whenMaybe (comparing nameW) <$> get (sortByWhitesName ctxMenu) checked
  ratingW' <- flip whenMaybe (comparing ratingW) <$> get (sortByWhitesRating ctxMenu) checked
  nameB' <- flip whenMaybe (comparing nameB) <$> get (sortByBlacksName ctxMenu) checked
  ratingB' <- flip whenMaybe (comparing ratingB) <$> get (sortByBlacksRating ctxMenu) checked
  gameType'' <- flip whenMaybe (comparing (gameType . settings)) <$> get (sortByGameType ctxMenu) checked

  return $ fromMaybe (comparing gameId)
           (gameId' <|> nameW' <|> ratingW' <|> nameB' <|> ratingB' <|> gameType'')

whenMaybe :: Bool -> a -> Maybe a
whenMaybe x = (guard x >>) . Just


getGamesOpts :: Menu () -> IO CtxMenu
getGamesOpts ctxMenu = CtxMenu
  <$> menuItem ctxMenu [ text := "Refresh" ]
  <*> menuItem ctxMenu [ text := "Show rated games", checkable := True, checked := True]
  <*> menuItem ctxMenu [ text := "Show unrated games", checkable := True, checked := True]
  <*> (menuLine ctxMenu >>
      menuRadioItem ctxMenu [ text := "Sort by Game Id"])
  <*> menuRadioItem ctxMenu [ text := "Sort by White's Name"]
  <*> menuRadioItem ctxMenu [ text := "Sort by White's Rating"]
  <*> menuRadioItem ctxMenu [ text := "Sort by Black's Name"]
  <*> menuRadioItem ctxMenu [ text := "Sort by Black's Rating"]
  <*> menuRadioItem ctxMenu [ text := "Sort by Game Type"]


toList :: Game -> [String]
toList g = [show $ Macbeth.Fics.Api.Game.gameId g, nameW g, show $ ratingW g, nameB g, show $ ratingB g, show $ gameType $ settings g]

