module Macbeth.Wx.Stored (
  widget
) where

import           Control.Concurrent.STM
import           Graphics.UI.WX hiding (refresh, widget)
import           Graphics.UI.WXCore hiding (widget)
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Stored
import           Macbeth.Fics.Message hiding (gameId)
import qualified Macbeth.Wx.Commands as Cmds
import           Macbeth.Wx.Utils
import           System.IO

data CtxMenu = CtxMenu {
    refresh :: MenuItem ()
}

widget :: Handle -> Panel () -> IO (ListCtrl (), Message -> IO ())
widget h p = do
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
  set storedListCtrl [on listEvent := onListEvent storedListCtrl h]

  storedMenuPane <- menuPane []
  storedMenuPopup <- ctxMenu storedMenuPane

  listItemRightClickEvent storedListCtrl $ \evt -> do
    pt <- listEventGetPoint evt
    menuPopup storedMenuPane pt storedListCtrl

  set (refresh storedMenuPopup) [on command := Cmds.stored h]
  return (storedListCtrl, handler storedListCtrl storedMenuPopup storedVar)


handler :: ListCtrl () -> CtxMenu -> TVar [Stored] -> Message -> IO ()
handler storedListCtrl ctx storedVar cmd = case cmd of
  StoredGames stored -> do
    atomically $ modifyTVar storedVar $ const stored
    set storedListCtrl [items := toList <$> stored]

  _ -> return ()


onListEvent :: ListCtrl() -> Handle -> EventList -> IO ()
onListEvent listCtrl h evt = case evt of
  ListItemActivated idx -> return () --listCtrlGetItemText gl idx >>= Cmds.observeGame h . read
  _ -> return ()


ctxMenu :: Menu () -> IO CtxMenu
ctxMenu ctxMenu = CtxMenu <$> menuItem ctxMenu [ text := "Refresh" ]


toList :: Stored -> [String]
toList Stored{..} = [show sId, show sColor, sOponent, show sOn, sType, sNext, sDate]


storedDummy = [Stored 1 White "TheDane" False "br  2  12" "0-0" "B2" "???" "Sun Nov 23,  6:14 CST 1997"]
