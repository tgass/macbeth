module Macbeth.Wx.Pending (
  wxPending
) where

import           Control.Concurrent.STM
import           Graphics.UI.WX
import           Graphics.UI.WXCore
import           Macbeth.Fics.Api.Offer
import           Macbeth.Fics.Api.Player
import qualified Macbeth.Fics.Commands as Cmds
import           Macbeth.Fics.Message
import           Macbeth.Wx.Utils
import           Macbeth.Wx.RuntimeEnv


data PendingActions =
    PendingTo { _accept :: MenuItem (), _decline :: MenuItem ()}
  | PendingFrom { _withdraw :: MenuItem () }


wxPending :: RuntimeEnv -> Panel () -> IO (Panel (), Message -> IO ())
wxPending env parentPanel = do
  p <- panel parentPanel []
  stFrom <- staticText p [ text := "Offers from other players:", fontSize := 12]
  lcFrom  <- listCtrl p [ columns := [
        ("#", AlignLeft, -1)
      , ("player", AlignLeft, -1)
      , ("offer type", AlignLeft, -1)
      , ("params", AlignLeft, 320)
      ]]
  vPending <- newTVarIO ([] :: [PendingOffer])

  stTo <- staticText p [ text := "Offers to other players:", fontSize := 12]
  lcTo  <- listCtrl p [columns := [
        ("#", AlignLeft, -1)
      , ("player", AlignLeft, -1)
      , ("offer type", AlignLeft, -1)
      , ("params", AlignLeft, 320)
      ]]

  set p [ layout := column 5 [ hfill $ widget stFrom
                             , fill $ widget lcFrom
                             , hfill $ widget $ vspace 5
                             , hfill $ widget $ hrule 1
                             , hfill $ widget $ vspace 5
                             , hfill $ widget stTo
                             , fill $ widget lcTo]]

  listItemRightClickEvent lcFrom (ctxMenuHandler env lcFrom From)
  listItemRightClickEvent lcTo (ctxMenuHandler env lcTo To)

  let handler cmd = case cmd of

                Pending offer -> do
                  atomically $ modifyTVar vPending (++ [offer])
                  itemAppend (if isFrom offer then lcFrom else lcTo) (toList offer)

                PendingRemoved idx -> do
                  items' <- atomically $ do
                    modifyTVar vPending $ filter ((/= idx) . offerId)
                    readTVar vPending
                  itemsDelete lcFrom
                  itemsDelete lcTo
                  mapM_ (itemAppend lcTo) (fmap toList (filter isTo items'))
                  mapM_ (itemAppend lcFrom) (fmap toList (filter isFrom items'))
                _ -> return ()

  return (p, handler)


toList :: PendingOffer -> [String]
toList (PendingOffer _ id' userHandle offerType' details') = [show id', name userHandle, offerType', show details']


ctxMenuHandler :: RuntimeEnv -> ListCtrl () -> Origin -> Graphics.UI.WXCore.ListEvent () -> IO ()
ctxMenuHandler env listCtrl' origin' evt = do
  ctxMenu <- menuPane []
  idx <- listEventGetIndex evt
  offerid <- (read . head . (!! idx)) <$> get listCtrl' items
  _ <- (if origin' == To then menuPendingTo else menuPendingFrom) ctxMenu env offerid
  when (idx >= 0) $ listEventGetPoint evt >>= flip (menuPopup ctxMenu) listCtrl'


menuPendingFrom :: Menu () -> RuntimeEnv -> Int -> IO PendingActions
menuPendingFrom ctxMenu env offerid = PendingTo
  <$> menuItem ctxMenu [ text := "Accept", on command := Cmds.acceptId env offerid]
  <*> menuItem ctxMenu [ text := "Decline", on command := Cmds.declineId env offerid]


menuPendingTo :: Menu () -> RuntimeEnv -> Int -> IO PendingActions
menuPendingTo ctxMenu env offerid = PendingFrom
  <$> menuItem ctxMenu [ text := "Withdraw", on command := Cmds.withdrawId env offerid]

