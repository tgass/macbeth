module Macbeth.Wx.Stored (
  widget
) where

import           Control.Concurrent.STM
import           Graphics.UI.WX hiding (refresh, widget)
import           Graphics.UI.WXCore hiding (widget)
import           Macbeth.Fics.Api.Stored
import           Macbeth.Fics.Message
import qualified Macbeth.Fics.Commands as Cmds
import           Macbeth.Wx.Utils
import qualified Macbeth.Wx.RuntimeEnv as E
import           Safe
import           System.IO

data CtxMenu = CtxMenu {
    resume :: MenuItem ()
  , refresh :: MenuItem ()
}

widget :: E.RuntimeEnv -> Panel () -> IO (ListCtrl (), Message -> IO ())
widget env p = do
  storedVar <- newTVarIO ([] :: [Stored])
  storedListCtrl  <- listCtrl p [ columns :=
      [ ("#", AlignLeft, -1)
      , ("Color", AlignLeft, -1)
      , ("Oponent", AlignLeft, -1)
      , ("On", AlignLeft, -1)
      , ("Type", AlignLeft, -1)
      , ("Next", AlignLeft, -1)
      , ("Date", AlignLeft, -1)
      ]]

  --listCtrlSetColumnWidths storedListCtrl 100
--  set storedListCtrl [on listEvent := onListEvent storedListCtrl h]

  storedMenuPane <- menuPane []
  storedMenuPopup <- ctxMenu storedMenuPane

  set (refresh storedMenuPopup) [on command := Cmds.stored env]

  listItemRightClickEvent storedListCtrl $ \evt -> do
    idx <- listEventGetIndex evt
    set (resume storedMenuPopup) [on command := do
      vals <- readTVarIO storedVar
      maybe (return ()) (Cmds.match env . sOponent) $ vals `atMay` idx
     ]
    listEventGetPoint evt >>= flip (menuPopup storedMenuPane) storedListCtrl

  return (storedListCtrl, handler storedListCtrl storedVar)


handler :: ListCtrl () -> TVar [Stored] -> Message -> IO ()
handler storedListCtrl storedVar cmd = case cmd of
  StoredGames stored -> do
    atomically $ modifyTVar storedVar $ const stored
    set storedListCtrl [items := toList <$> stored]
  _ -> return ()


onListEvent :: ListCtrl() -> Handle -> EventList -> IO ()
onListEvent listCtrl h evt = case evt of
  ListItemActivated idx -> return () --listCtrlGetItemText gl idx >>= Cmds.observeGame h . read
  _ -> return ()


ctxMenu :: Menu () -> IO CtxMenu
ctxMenu ctxMenu = CtxMenu 
  <$> menuItem ctxMenu [ text := "Resume" ]
  <*> menuItem ctxMenu [ text := "Refresh" ]


toList :: Stored -> [String]
toList Stored{..} = [show sId, show sColor, sOponent, show sOn, sType, sNext, sDate]


