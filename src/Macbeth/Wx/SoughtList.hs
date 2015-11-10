module Macbeth.Wx.SoughtList (
  wxSoughtList
) where

import Macbeth.Api.Seek
import Macbeth.Api.Game hiding (gameType)
import Macbeth.Api.CommandMsg

import Control.Monad
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import System.IO
import Data.List (elemIndex)

supportedGameTypes = [Untimed, Standard, Blitz, Lightning]

wxSoughtList :: Panel () -> Handle -> IO (ListCtrl (), CommandMsg -> IO CommandMsg)
wxSoughtList slp h = do
    sl  <- listCtrl slp [columns := [ ("#", AlignLeft, -1)
                                    , ("handle", AlignLeft, -1)
                                    , ("rating", AlignLeft, -1)
                                    , ("Time (start inc.)", AlignRight, -1)
                                    , ("type", AlignRight, -1)]
                                    ]
    set sl [on listEvent := onSeekListEvent sl h]
    listCtrlSetColumnWidths sl 100
    let handler cmd = case cmd of
            NewSeek seek -> do
                              when (gameType seek `elem` supportedGameTypes) (itemAppend sl $ toList seek)
                              return cmd

            ClearSeek -> itemsDelete sl >> return cmd

            RemoveSeeks gameIds -> do
              seeks <- get sl items
              sequence_ $ fmap (deleteSeek sl . findSeekIdx seeks) gameIds
              return cmd
            _ -> return cmd

    return (sl, handler)


onSeekListEvent :: ListCtrl() -> Handle -> EventList -> IO ()
onSeekListEvent sl h eventList = case eventList of
  ListItemActivated idx -> do
    seeks <- get sl items
    hPutStrLn h $ "4 play " ++ show (read $ head (seeks !! idx) :: Int)
  _ -> return ()


findSeekIdx :: [[String]] -> Int -> Maybe Int
findSeekIdx seeks gameId = elemIndex gameId $ fmap (read . (!! 0)) seeks


deleteSeek :: ListCtrl () -> Maybe Int -> IO ()
deleteSeek sl (Just id) = itemDelete sl id
deleteSeek _ _ = return ()


toList :: Seek -> [String]
toList (Seek id name rating time inc isRated gameType _ _) =
  [show id, name, show rating, show time ++ " " ++ show inc, show gameType ++ " " ++ showIsRated isRated]
  where showIsRated True = "rated"
        showIsRated False = "unrated"
