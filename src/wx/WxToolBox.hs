{-# LANGUAGE OverloadedStrings #-}

module WxToolBox (
  createToolBox
) where

import Api
import CommandMsg
import Game
import Seek
import WxAbout
import WxMatch
import WxSeek
import WxUtils

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.List
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO


ficsEventId :: Int
ficsEventId = wxID_HIGHEST + 51

-- TODO: refresh game list every minute
-- TODO: filter sought list (blitz, standard only)
createToolBox :: Handle -> String -> Bool -> Chan CommandMsg -> IO ()
createToolBox h name isGuest chan = do
    vCmd <- newEmptyMVar

    -- main frame
    f  <- frame []
    status <- statusField [text := "Logged in as " ++ name]

    -- right panel
    right <- panel f []
    nb <- notebook right []

    -- tab1 : Sought list
    slp <- panel nb []
    sl  <- listCtrl slp [columns := [ ("#", AlignLeft, -1)
                                    , ("handle", AlignLeft, -1)
                                    , ("rating", AlignLeft, -1)
                                    , ("Time (start inc.)", AlignRight, -1)
                                    , ("type", AlignRight, -1)]
                                    ]
    set sl [on listEvent := onSeekListEvent sl h]
    listCtrlSetColumnWidths sl 100

    -- toolbar
    tbar   <- toolBar f []
    toolItem tbar "Seek" False  "/Users/tilmann/Documents/leksah/XChess/gif/volume-up.png" [ on command := wxSeek h isGuest ]
    toolItem tbar "Match" False  "/Users/tilmann/Documents/leksah/XChess/gif/dot-circle-o.png" [ on command := wxMatch h isGuest ]


    -- tab2 : console
    cp <- panel nb []
    ct <- textCtrlEx cp (wxTE_MULTILINE .+. wxTE_RICH) [font := fontFixed]
    ce <- entry cp []
    set ce [on enterKey := emitCommand ce h]


    -- tab3 : Games list
    glp <- panel nb []
    gl  <- listCtrl glp [columns := [("#", AlignLeft, -1)
                                    , ("player 1", AlignLeft, -1)
                                    , ("rating", AlignLeft, -1)
                                    , ("player 2", AlignLeft, -1)
                                    , ("rating", AlignLeft, -1)
                                    ]
                        ]
    listCtrlSetColumnWidths gl 100

    -- about menu
    m_help    <- menuHelp      []
    m_about  <- menuAbout m_help [help := "About XChess", on command := wxAbout ]

    set f [ layout := (container right $
                         column 0
                         [ tabs nb
                            [ tab "Sought" $ container slp $ fill $ widget sl
                            , tab "Games" $ container glp $ fill $ widget gl
                            , tab "Console" $ container cp $
                                            ( column 5  [ floatLeft $ expand $ hstretch $ widget ct
                                                        , expand $ hstretch $ widget ce])
                            ]
                         ]
            )
          , menuBar := [m_help]
          , statusBar := [status]
          ]

    threadId <- forkIO $ (eventLoop ficsEventId) chan vCmd f

    windowOnDestroy f $ killThread threadId

    evtHandlerOnMenuCommand f ficsEventId $ takeMVar vCmd >>= \cmd -> do
      case cmd of

        Games games -> do
          set gl [items := [[show id, n1, show r1, n2, show r2] | (Game id _ _ r1 n1 r2 n2 _) <- games]]
          set gl [on listEvent := onGamesListEvent games h]

        NewSeek seek -> itemAppend sl $ toList seek

        ClearSeek -> itemsDelete sl

        RemoveSeeks gameIds -> do
          seeks <- get sl items
          sequence_ $ fmap (deleteSeek sl . findSeekIdx seeks) gameIds

        SettingsDone -> hPutStrLn h "4 iset seekinfo 1" >> hPutStrLn h "4 games"

        TextMessage text -> appendText ct $ text ++ "\n"

        _ -> return ()


findSeekIdx :: [[String]] -> Int -> Maybe Int
findSeekIdx seeks gameId = elemIndex gameId $ fmap (read . (!! 0)) seeks

deleteSeek :: ListCtrl () -> Maybe Int -> IO ()
deleteSeek sl (Just id) = itemDelete sl id
deleteSeek _ _ = return ()

toList :: Seek -> [String]
toList (Seek id name rating time inc isRated gameType color ratingRange) =
  [show id, name, show rating, (show time ++ " " ++ show inc), show gameType ++ " " ++ showIsRated isRated]
  where showIsRated True = "rated"
        showIsRated False = "unrated"

onGamesListEvent :: [Game] -> Handle -> EventList -> IO ()
onGamesListEvent games h eventList = case eventList of
  ListItemActivated idx -> hPutStrLn h $ "4 observe " ++ show (Game.id $ games !! idx)
  _ -> return ()


onSeekListEvent :: ListCtrl() -> Handle -> EventList -> IO ()
onSeekListEvent sl h eventList = case eventList of
  ListItemActivated idx -> do
    seeks <- get sl items
    hPutStrLn h $ "4 play " ++ show (read $ seeks !! idx !! 0 :: Int)
  _ -> return ()


emitCommand :: TextCtrl () -> Handle -> IO ()
emitCommand textCtrl h = get textCtrl text >>= hPutStrLn h >> set textCtrl [text := ""]

