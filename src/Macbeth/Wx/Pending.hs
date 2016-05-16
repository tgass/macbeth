module Macbeth.Wx.Pending (
  wxPending
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.PendingOffer
import Macbeth.Fics.Api.Player
import Macbeth.Wx.Utils

import Control.Concurrent.STM
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO


data PendingActions =
    PendingTo { accept :: MenuItem (), decline :: MenuItem ()}
  | PendingFrom { withdraw :: MenuItem () }


wxPending :: Handle -> Panel () -> IO (Panel (), FicsMessage -> IO ())
wxPending h p' = do
  p <- panel p' []
  stFrom <- staticText p [ text := "Offers from other players:", fontSize := 12]
  lcFrom  <- listCtrl p [ columns := [
        ("#", AlignLeft, -1)
      , ("player", AlignLeft, -1)
      , ("offer type", AlignLeft, -1)
      , ("params", AlignLeft, -1)
      ]]
  vPending <- newTVarIO ([] :: [PendingOffer])

  stTo <- staticText p [ text := "Offers to other players:", fontSize := 12]
  lcTo  <- listCtrl p [columns := [
        ("#", AlignLeft, -1)
      , ("player", AlignLeft, -1)
      , ("offer type", AlignLeft, -1)
      , ("params", AlignLeft, -1)
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
                  items <- atomically $ do
                    modifyTVar vPending $ filter ((/= idx) . offerId)
                    readTVar vPending
                  itemsDelete lcFrom
                  itemsDelete lcTo
                  mapM_ (itemAppend lcTo) (fmap toList (filter isTo items))
                  mapM_ (itemAppend lcFrom) (fmap toList (filter isFrom items))
                _ -> return ()

  return (p, handler)


toList :: PendingOffer -> [String]
toList (PendingOffer _ id userHandle offerType params) = [show id, name userHandle, offerType, params]


ctxMenuHandler :: Handle -> ListCtrl () -> Origin -> Graphics.UI.WXCore.ListEvent () -> IO ()
ctxMenuHandler h listCtrl origin evt = do
  ctxMenu <- menuPane []
  idx <- listEventGetIndex evt
  offerId <- (head . (!! idx)) <$> get listCtrl items
  _ <- (if origin == To then menuPendingTo else menuPendingFrom) ctxMenu h offerId
  when (idx >= 0) $ listEventGetPoint evt >>= flip (menuPopup ctxMenu) listCtrl


menuPendingFrom :: Menu () -> Handle -> String -> IO PendingActions
menuPendingFrom ctxMenu h offerId = PendingTo
  <$> menuItem ctxMenu [ text := "Accept", on command := hPutStrLn h ("4 accept " ++ offerId)]
  <*> menuItem ctxMenu [ text := "Decline", on command := hPutStrLn h ("4 decline " ++ offerId)]


menuPendingTo :: Menu () -> Handle -> String -> IO PendingActions
menuPendingTo ctxMenu h offerId = PendingFrom
  <$> menuItem ctxMenu [ text := "Withdraw", on command := hPutStrLn h ("4 withdraw " ++ offerId)]

