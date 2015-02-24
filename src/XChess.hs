{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
import Seek
import Game
import WxObservedGame
import FicsConnection2 (ficsConnection)
import CommandMsg

import Control.Applicative (liftA)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, forkIO)
import Control.Concurrent.Chan

import qualified Data.ByteString.Char8 as BS

import Graphics.UI.WX
import Graphics.UI.WXCore

import System.IO (Handle, hPutStrLn)

type Position = [(Square, Piece)]



main = do
  chan <- newChan
  h <- ficsConnection $ \cmd -> do
    writeChan chan cmd
  start $ gui h chan



ficsEventId = wxID_HIGHEST + 51



gui h chan = do
    vCmd <- newEmptyMVar

    -- main frame
    f  <- frame []
    mv <- newEmptyMVar

    -- right panel
    right <- panel f []
    nb <- notebook right []

    -- tab1 : Sought list
    slp <- panel nb []
    sl  <- listCtrl slp [columns := [ ("#", AlignLeft, -1)
                                    , ("handle", AlignLeft, -1)
                                    , ("rating", AlignLeft, -1)
                                    , ("game type", AlignRight, -1)]
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
    set gl [on listEvent := onGamesListEvent gl h]

    set f [layout := (container right $
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
          ]

    evtHandlerOnMenuCommand f ficsEventId $ takeMVar vCmd >>= \cmd -> do
      putStrLn $ show cmd
      case cmd of
        SoughtMsg _ seeks -> do
          set sl [items := [[show id, name, show rat, show gt] | (Seek id rat name _ _ _ gt _ _) <- seeks]]

        GamesMsg _ games -> do
          set gl [items := [[show id, n1, show r1, n2, show r2] | (Game id _ r1 n1 r2 n2 _) <- games]]

        ObserveMsg _ move -> do
          chan' <- dupChan chan
          createObservedGame move chan'
          return ()

        LoginMessage     -> hPutStrLn h "guest"

        PasswordMessage  -> hPutStrLn h "efgeon"

        LoggedInMessage  -> hPutStrLn h "set seek 0" >>
                            hPutStrLn h "set style 12" >>
                            hPutStrLn h "iset nowrap 1" >>
                            hPutStrLn h "iset block 1"

        SettingsDoneMsg  -> hPutStrLn h "5 sought" >>
                            hPutStrLn h "4 games"

        TextMessage text -> appendText ct (BS.unpack text ++ "\n")

        _                -> return ()


    forkIO $ loop chan vCmd f
    return ()



loop :: Chan CommandMsg -> MVar CommandMsg -> Frame () -> IO ()
loop chan vCmd f = do
  cmd <- readChan chan
  putMVar vCmd cmd
  ev <- commandEventCreate wxEVT_COMMAND_MENU_SELECTED ficsEventId
  evtHandlerAddPendingEvent f ev
  loop chan vCmd f



onGamesListEvent :: t -> Handle -> EventList -> IO ()
onGamesListEvent list h eventList = case eventList of
  ListItemActivated idx -> hPutStrLn h $ "4 observe " ++ show idx
  _ -> return ()



emitCommand :: TextCtrl () -> Handle -> IO ()
emitCommand e h = do
  cmd <- get e text
  set e [text := ""]
  hPutStrLn h cmd
