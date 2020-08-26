module Macbeth.Wx.Pending (
  wxPending
) where

import           Control.Concurrent.STM
import           Graphics.UI.WX
import           Graphics.UI.WXCore
import qualified Macbeth.Fics.Commands as Cmds
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Offer
import           Macbeth.Fics.Api.Player
import           Macbeth.Wx.Utils
import           System.IO


data PendingActions =
    PendingTo { _accept :: MenuItem (), _decline :: MenuItem ()}
  | PendingFrom { _withdraw :: MenuItem () }


wxPending :: Handle -> Panel () -> IO (Panel (), Message -> IO ())
wxPending h p' = do
  p <- panel p' []
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

  listItemRightClickEvent lcFrom (ctxMenuHandler h lcFrom From)
  listItemRightClickEvent lcTo (ctxMenuHandler h lcTo To)

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


ctxMenuHandler :: Handle -> ListCtrl () -> Origin -> Graphics.UI.WXCore.ListEvent () -> IO ()
ctxMenuHandler h listCtrl' origin' evt = do
  ctxMenu <- menuPane []
  idx <- listEventGetIndex evt
  offerid <- (read . head . (!! idx)) <$> get listCtrl' items
  _ <- (if origin' == To then menuPendingTo else menuPendingFrom) ctxMenu h offerid
  when (idx >= 0) $ listEventGetPoint evt >>= flip (menuPopup ctxMenu) listCtrl'


menuPendingFrom :: Menu () -> Handle -> Int -> IO PendingActions
menuPendingFrom ctxMenu h offerid = PendingTo
  <$> menuItem ctxMenu [ text := "Accept", on command := Cmds.acceptId h offerid]
  <*> menuItem ctxMenu [ text := "Decline", on command := Cmds.declineId h offerid]


menuPendingTo :: Menu () -> Handle -> Int -> IO PendingActions
menuPendingTo ctxMenu h offerid = PendingFrom
  <$> menuItem ctxMenu [ text := "Withdraw", on command := Cmds.withdrawId h offerid]

