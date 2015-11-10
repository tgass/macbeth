module Macbeth.Wx.Pending (
  wxPending
) where

import Macbeth.Api.Api
import Macbeth.Api.CommandMsg
import Macbeth.Wx.Utils

import Control.Applicative
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO


data PendingActions = PendingActions { accept :: MenuItem (), decline :: MenuItem ()}


wxPending :: Handle -> Panel () -> IO (Panel (), CommandMsg -> IO ())
wxPending h p' = do
  p <- panel p' []
  stFrom <- staticText p [ text := "Offers from other players:", fontSize := 12]
  lcFrom  <- listCtrl p [ columns := [ ("#", AlignLeft, -1), ("offer", AlignLeft, -1)]]

  stTo <- staticText p [ text := "Offers to other players:", fontSize := 12]
  lcTo  <- listCtrl p [columns := [ ("#", AlignLeft, -1), ("offer", AlignLeft, -1)]]

  set p [ layout := column 5 [ hfill $ widget stFrom
                             , fill $ widget lcFrom
                             , hfill $ widget $ vspace 5
                             , hfill $ widget $ hrule 1
                             , hfill $ widget $ vspace 5
                             , hfill $ widget stTo
                             , fill $ widget lcTo]]

  let handler cmd = case cmd of

                PendingOffers tx fx -> do
                  set lcFrom [items := [ toList offer | offer <- fx]]
                  listItemRightClickEvent lcFrom (ctxMenuHandler h lcFrom fx)
                  set lcTo [items := [ toList offer | offer <- tx]]

                OfferAccepted -> hPutStrLn h "4 pending"

                OfferDeclined -> hPutStrLn h "4 pending"

                _ -> return ()

  return (p, handler)


toList :: PendingOffer -> [String]
toList (PendingOffer id offer) = [show id, offer]


getPendingActions ctxMenu = PendingActions
  <$> menuItem ctxMenu [ text := "Accept" ]
  <*> menuItem ctxMenu [ text := "Decline" ]


ctxMenuHandler :: Handle -> ListCtrl () -> [PendingOffer] -> Graphics.UI.WXCore.ListEvent () -> IO ()
ctxMenuHandler h listCtrl offerList evt = do
  ctxMenu <- menuPane []
  opts <- getPendingActions ctxMenu
  idx <- listEventGetIndex evt
  set (accept opts) [on command := hPutStrLn h ("4 accept " ++ show (offerId $ offerList !! idx))
                                >> hPutStrLn h "4 pending"]
  set (decline opts) [on command := hPutStrLn h ("4 decline " ++ show (offerId $ offerList !! idx))
                                 >> hPutStrLn h "4 pending"]
  pt <- listEventGetPoint evt
  menuPopup ctxMenu pt listCtrl

