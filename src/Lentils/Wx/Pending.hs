module Lentils.Wx.Pending (
  wxPending
) where

import Lentils.Api.Api
import Lentils.Api.CommandMsg
import Graphics.UI.WX


wxPending :: Panel () -> IO (Panel (), CommandMsg -> IO CommandMsg)
wxPending p' = do
  p <- panel p' []
  stFrom <- staticText p [ text := "Offers from other players:", fontSize := 12]
  lcFrom  <- listCtrl p [ columns := [ ("#", AlignLeft, -1)
                                     , ("offer", AlignLeft, -1)
                                     ]
                        ]
  stTo <- staticText p [ text := "Offers to other players:", fontSize := 12]

  lcTo  <- listCtrl p [columns := [ ("#", AlignLeft, -1)
                                , ("offer", AlignLeft, -1)
                                ]
                    ]

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
                  set lcTo [items := [ toList offer | offer <- tx]]
                  return cmd
                _ -> return cmd

  return (p, handler)


toList :: PendingOffer -> [String]
toList (PendingOffer id offer) = [show id, offer]

