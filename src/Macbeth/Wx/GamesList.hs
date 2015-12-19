module Macbeth.Wx.GamesList (
  wxGamesList
) where

import Macbeth.Api.Game
import Macbeth.Api.CommandMsg
import Macbeth.Wx.Utils

import Control.Applicative
import Graphics.UI.WX hiding (refresh)
import Graphics.UI.WXCore
import System.IO

data GamesOpts = GamesOpts { refresh :: MenuItem ()
                           , showRated :: MenuItem ()
                           , showUnrated :: MenuItem ()
                           }

wxGamesList :: Panel () -> Handle -> IO (ListCtrl (), CommandMsg -> IO ())
wxGamesList glp h = do
  gl  <- listCtrl glp [ columns :=
      [ ("#", AlignLeft, -1)
      , ("player 1", AlignLeft, -1)
      , ("rating", AlignLeft, -1)
      , ("player 2", AlignLeft, -1)
      , ("rating", AlignLeft, -1)
      , ("Game type", AlignLeft, -1)
      ]]
  listCtrlSetColumnWidths gl 100

  glCtxMenu <- menuPane []
  gamesOpts <- getGamesOpts glCtxMenu
  set (refresh gamesOpts) [on command := hPutStrLn h "4 games"]
  return (gl, handler gamesOpts gl glCtxMenu h)


handler :: GamesOpts -> ListCtrl () -> Menu () -> Handle -> CommandMsg -> IO ()
handler gamesOpts gl glCtxMenu h cmd = case cmd of
  Games games -> do
    filterGamesList gl gamesOpts games
    set (showRated gamesOpts) [on command := filterGamesList gl gamesOpts games]
    set (showUnrated gamesOpts) [on command := filterGamesList gl gamesOpts games]
    set gl [on listEvent := onGamesListEvent gl h]

    listItemRightClickEvent gl (\evt -> do
      pt <- listEventGetPoint evt
      menuPopup glCtxMenu pt gl)
  _ -> return ()


onGamesListEvent :: ListCtrl() -> Handle -> EventList -> IO ()
onGamesListEvent gl h evt = case evt of
  ListItemActivated idx -> listCtrlGetItemText gl idx >>= hPutStrLn h . ("4 observe " ++)
  _ -> return ()


filterGamesList :: ListCtrl () -> GamesOpts -> [Game] -> IO ()
filterGamesList gl opts games = do
  showRated' <- get (showRated opts) checked
  showUnrated' <- get (showUnrated opts) checked
  set gl [items := [ toList g | g <- games
                   , (isRated (settings g) == showRated') ||
                     (isRated (settings g) == not showUnrated')
                   , not $ isPrivate $ settings g]]


getGamesOpts :: Menu () -> IO GamesOpts
getGamesOpts ctxMenu = GamesOpts
  <$> menuItem ctxMenu [ text := "Refresh" ]
  <*> menuItem ctxMenu [ text := "Show rated games", checkable := True, checked := True]
  <*> menuItem ctxMenu [ text := "Show unrated games", checkable := True, checked := True]


toList :: Game -> [String]
toList g = [show $ Macbeth.Api.Game.id g, nameW g, show $ ratingW g, nameB g, show $ ratingB g, show $ gameType $ settings g]

