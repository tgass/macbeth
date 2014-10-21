module SeekList (
) where

import Seek
import Graphics.UI.WX
import Graphics.UI.WXCore

entries = [Seek 1 1900 "foobar" Blitz 100 1 False (Just White) (1800, 2000)
          ,Seek 2 2563 "jazzer" Standard 100 1 False (Just White) (2500, 2800)
          ]

onListEvent list eventList
  = case eventList of
      ListItemSelected idx    -> do
                                  listCtrlGetItemText list idx >>= logMessage . (++) "item selected: "
                                  set list [items :=
                                    [[handle, show rating, show gt] | (Seek _ rating handle gt _ _ _ _ _) <-
                                      [Seek 3 2100 "barbaz" Blitz 100 1 False (Just White) (1800, 2000)]]]

      ListItemDeselected idx  -> logMessage ("item de-selected: " ++ show idx)
      other                   -> logMessage ("list control event.")

