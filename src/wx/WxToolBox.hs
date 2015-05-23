{-# LANGUAGE OverloadedStrings #-}

module WxToolBox (
  createToolBox
) where

import Api
import CommandMsg
import Game
import Seek
import WxMenu

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.List
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO


ficsEventId :: Int
ficsEventId = wxID_HIGHEST + 51


createToolBox :: Handle -> String -> Chan CommandMsg -> IO ()
createToolBox h name chan = do
    vCmd <- newEmptyMVar

    -- main frame
    f  <- frame []
    menu <- wxMenu h
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
                                    , ("game type", AlignRight, -1)
                                    , ("isRated", AlignLeft, -1)]
                                    ]


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

    set f [ layout := minsize (Size 400 600) $ (container right $
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
          , menuBar := [menu]
          , statusBar := [status]
          ]

    threadId <- forkIO $ loop chan vCmd f

    windowOnDestroy f $ killThread threadId

    evtHandlerOnMenuCommand f ficsEventId $ takeMVar vCmd >>= \cmd -> do
      case cmd of

        Games games -> do
          set gl [items := [[show id, n1, show r1, n2, show r2] | (Game id _ _ r1 n1 r2 n2 _) <- games]]
          set gl [on listEvent := onGamesListEvent games h]

        NewSeek (Seek id name rating time inc isRated gameType color ratingRange) -> do
          itemAppend sl [show id, name, show rating, (show time ++ " " ++ show inc), show gameType, show isRated]
          set sl [on listEvent := onSeekListEvent sl h]

        ClearSeek -> itemsDelete sl

        RemoveSeeks gameIds -> do
          seeks <- get sl items
          sequence_ $ fmap (deleteSeek sl . findSeekIdx seeks) gameIds

        SettingsDone -> hPutStrLn h "4 iset seekinfo 1" >> hPutStrLn h "4 games"

        TextMessage text -> appendText ct $ text ++ "\n"

        Prompt -> return ()

        cmd -> appendText ct (show cmd ++ "\n")

findSeekIdx :: [[String]] -> Int -> Maybe Int
findSeekIdx seeks gameId = elemIndex gameId $ fmap (read . (!! 0)) seeks


deleteSeek :: ListCtrl () -> Maybe Int -> IO ()
deleteSeek sl (Just id) = itemDelete sl id
deleteSeek _ _ = return ()


loop :: Chan CommandMsg -> MVar CommandMsg -> Frame () -> IO ()
loop chan vCmd f = readChan chan >>= putMVar vCmd >>
  commandEventCreate wxEVT_COMMAND_MENU_SELECTED ficsEventId >>= evtHandlerAddPendingEvent f >>
  loop chan vCmd f


onGamesListEvent :: [Game] -> Handle -> EventList -> IO ()
onGamesListEvent games h eventList = case eventList of
  ListItemActivated idx -> hPutStrLn h $ "4 observe " ++ show (Game.id $ games !! idx)
  _ -> return ()


onSeekListEvent :: ListCtrl() -> Handle -> EventList -> IO ()
onSeekListEvent sl h eventList = case eventList of
  ListItemActivated idx -> do
                            seeks <- get sl items
                            hPutStrLn h $ "4 play " ++ show (read (seeks !! idx !! 0) :: Int)
  _ -> return ()


emitCommand :: TextCtrl () -> Handle -> IO ()
emitCommand textCtrl h = get textCtrl text >>= hPutStrLn h >> set textCtrl [text := ""]

