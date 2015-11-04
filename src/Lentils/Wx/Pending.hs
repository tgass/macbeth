module Lentils.Wx.Pending (
  wxPending
) where

import Graphics.UI.WX


wxPending :: Panel () -> IO (Panel ())
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
  return p

--"\NAK5\SYN87\SYNThere are no offers pending to other players.\n\nThere are no offers pending from other players.\n\ETB"
