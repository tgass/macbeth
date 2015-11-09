module Lentils.Wx.GamesList (
  wxGamesList
) where

import Lentils.Api.Game
import Lentils.Api.CommandMsg
import Lentils.Wx.Utils

import Control.Applicative
import Graphics.UI.WX hiding (refresh)
import Graphics.UI.WXCore
import System.IO

data GamesOpts = GamesOpts { refresh :: MenuItem ()
                           , showRated :: MenuItem ()
                           , showUnrated :: MenuItem ()
                           }

wxGamesList :: Panel () -> Handle -> IO (ListCtrl (), CommandMsg -> IO CommandMsg)
wxGamesList glp h = do
  gl  <- listCtrl glp [
    columns :=
      [ ("#", AlignLeft, -1)
      , ("player 1", AlignLeft, -1)
      , ("rating", AlignLeft, -1)
      , ("player 2", AlignLeft, -1)
      , ("rating", AlignLeft, -1)
      , ("Game type", AlignLeft, -1)
      ]
    ]
  listCtrlSetColumnWidths gl 100

  glCtxMenu <- menuPane []
  gamesOpts <- getGamesOpts glCtxMenu
  set (refresh gamesOpts) [on command := hPutStrLn h "4 games"]

  let handler cmd = case cmd of
          Games games -> do
              filterGamesList gl gamesOpts games
              set (showRated gamesOpts) [on command := filterGamesList gl gamesOpts games]
              set (showUnrated gamesOpts) [on command := filterGamesList gl gamesOpts games]
              set gl [on listEvent := onGamesListEvent games h]

              listItemRightClickEvent gl (\evt -> do
                pt <- listEventGetPoint evt
                menuPopup glCtxMenu pt gl)
              return cmd
          _ -> return cmd

  return (gl, handler)


onGamesListEvent :: [Game] -> Handle -> EventList -> IO ()
onGamesListEvent games h eventList = case eventList of
  ListItemActivated idx -> hPutStrLn h $ "4 observe " ++ show (Lentils.Api.Game.id $ games !! idx)
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


toList g = [show $ Lentils.Api.Game.id g, nameW g, show $ ratingW g, nameB g, show $ ratingB g, show $ gameType $ settings g]

