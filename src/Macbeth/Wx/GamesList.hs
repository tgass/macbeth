module Macbeth.Wx.GamesList (
  wxGamesList
) where


import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Graphics.UI.WX hiding (refresh)
import           Graphics.UI.WXCore
import qualified Macbeth.Fics.Commands as Cmds
import           Macbeth.Fics.Api.OngoingGame
import           Macbeth.Fics.Message hiding (gameId)
import           Macbeth.Wx.Utils
import           System.IO

data CtxMenu = CtxMenu {
    refresh :: MenuItem ()
  , showRated :: MenuItem ()
  , showUnrated :: MenuItem ()
  , _sortBy :: MenuItem ()
}

data CtxSortMenu = CtxSortMenu {
    sortByGameId :: MenuItem ()
  , sortByNameW :: MenuItem ()
  , sortByRatingW :: MenuItem ()
  , sortByNameB :: MenuItem ()
  , sortByRatingB :: MenuItem ()
  , sortByGameType :: MenuItem ()
}


wxGamesList :: Panel () -> Handle -> IO (ListCtrl (), Message -> IO ())
wxGamesList glp h = do
  games <- newTVarIO ([] :: [OngoingGame])
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
  glCtxSub <- menuPane []

  ctxSortMenu' <- ctxSortMenu glCtxSub
  ctxMenuPopup <- ctxMenu glCtxMenu glCtxSub

  listItemRightClickEvent gl $ \evt -> do
    pt' <- listEventGetPoint evt
    menuPopup glCtxMenu pt' gl

  set (refresh ctxMenuPopup) [on command := Cmds.games h]


  set (showRated ctxMenuPopup) [on command := displayGames gl ctxMenuPopup ctxSortMenu' games]
  set (showUnrated ctxMenuPopup) [on command := displayGames gl ctxMenuPopup ctxSortMenu' games]
  set glp [ on (menu $ sortByGameId ctxSortMenu') := displayGames gl ctxMenuPopup ctxSortMenu' games ]
  set glp [ on (menu $ sortByNameW ctxSortMenu') := displayGames gl ctxMenuPopup ctxSortMenu' games ]
  set glp [ on (menu $ sortByRatingW ctxSortMenu') := displayGames gl ctxMenuPopup ctxSortMenu' games ]
  set glp [ on (menu $ sortByNameB ctxSortMenu') := displayGames gl ctxMenuPopup ctxSortMenu' games ]
  set glp [ on (menu $ sortByRatingB ctxSortMenu') := displayGames gl ctxMenuPopup ctxSortMenu' games ]
  set glp [ on (menu $ sortByGameType ctxSortMenu') := displayGames gl ctxMenuPopup ctxSortMenu' games ]


  return (gl, handler gl ctxMenuPopup ctxSortMenu' games)


handler :: ListCtrl () -> CtxMenu -> CtxSortMenu -> TVar [OngoingGame] -> Message -> IO ()
handler gl ctx sub games cmd = case cmd of
  Games games' -> do
    atomically $ modifyTVar games (const games')
    displayGames gl ctx sub games

  _ -> return ()


onGamesListEvent :: ListCtrl() -> Handle -> EventList -> IO ()
onGamesListEvent gl h evt = case evt of
  ListItemActivated idx -> listCtrlGetItemText gl idx >>= Cmds.observeGame h . read
  _ -> return ()


displayGames :: ListCtrl () -> CtxMenu -> CtxSortMenu -> TVar [OngoingGame] -> IO ()
displayGames gl opts sub games = do
  sortOrder <- sortFoo sub
  games' <- sortBy sortOrder <$> readTVarIO games

  showRated' <- get (showRated opts) checked
  showUnrated' <- get (showUnrated opts) checked
  set gl [items := [ toList g | g <- games'
                   , (isRated (settings g) == showRated') ||
                     (isRated (settings g) == not showUnrated')
                   , not $ isPrivate $ settings g]]


sortFoo :: CtxSortMenu -> IO (OngoingGame -> OngoingGame -> Ordering)
sortFoo ctxMenu' = do
  gameId'' <- foo gameId (sortByGameId ctxMenu')
  nameW' <- foo nameW (sortByNameW ctxMenu')
  ratingW' <- foo (Down . ratingW) (sortByRatingW ctxMenu')
  nameB' <- foo nameB (sortByNameB ctxMenu')
  ratingB' <- foo (Down . ratingB) (sortByRatingB ctxMenu')
  gameType'' <- foo (gameType . settings) (sortByGameType ctxMenu')

  return $ fromMaybe (comparing gameId)
           (gameId'' <|> nameW'  <|> ratingW' <|> nameB' <|> ratingB' <|> gameType'')


foo :: Ord a => (OngoingGame -> a) -> MenuItem () -> IO (Maybe (OngoingGame -> OngoingGame -> Ordering))
foo f mu = flip whenMaybe (comparing f) <$> get mu checked


whenMaybe :: Bool -> a -> Maybe a
whenMaybe x = (guard x >>) . Just


ctxMenu :: Menu () -> Menu () -> IO CtxMenu
ctxMenu ctxMenu' sub = CtxMenu
  <$> menuItem ctxMenu' [ text := "Refresh" ]
  <*> menuItem ctxMenu' [ text := "Show rated games", checkable := True, checked := True]
  <*> menuItem ctxMenu' [ text := "Show unrated games", checkable := True, checked := True]
  <*> menuSub ctxMenu' sub [ text := "Sort By" ]


ctxSortMenu :: Menu () -> IO CtxSortMenu
ctxSortMenu menu' = CtxSortMenu
  <$> menuRadioItem menu' [ text := "Game Id" ]
  <*> menuRadioItem menu' [ text := "White's Name"]
  <*> menuRadioItem menu' [ text := "White's Rating"]
  <*> menuRadioItem menu' [ text := "Black's Name"]
  <*> menuRadioItem menu' [ text := "Black's Rating"]
  <*> menuRadioItem menu' [ text := "Game Type"]


toList :: OngoingGame -> [String]
toList g = [show $ Macbeth.Fics.Api.OngoingGame.gameId g, nameW g, show $ ratingW g, nameB g, show $ ratingB g, show $ gameType $ settings g]

