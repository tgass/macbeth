{-# LANGUAGE OverloadedStrings #-}

module Main where

import Board
import FicsConnection2 (ficsConnection)
import CommandMsgParser
import Seek
import SeekParser
import PositionParser

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO (Handle, hPutStrLn)

main = start gui

ficsEventId = wxID_HIGHEST + 51

gui = do
    -- main frame
    f  <- frame []
    mv <- newEmptyMVar
    h <- ficsConnection $ handler f mv

    -- split main frame in panel left / right
    s <- splitterWindow f []



    -- left panel: board
    boardVar <- variable [ value := [] ]
    left <- panel s [on paint := drawAll boardVar]



    -- right panel: notebook
    right <- panel s []
    nb <- notebook right []

    -- tab1 : Sought list
    slp <- panel nb []
    sl  <- listCtrl slp [columns := [ ("handle", AlignLeft, -1)
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
    gl  <- listCtrl glp [columns := [ ("player 1", AlignLeft, -1)
                                    , ("rating", AlignLeft, -1)
                                    , ("player 2", AlignLeft, -1)
                                    , ("rating", AlignLeft, -1)
                                    , ("game type", AlignLeft, -1)]
                                    ]


    set f [layout := container s $ fill $ vsplit s 15 400
                      (container left $ space 320 320)
                      (container right $
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
          , clientSize := sz 600 320
          ]
    registerFicsEvents f (action h mv ct sl left boardVar)
    return ()



handler :: Frame () -> MVar CommandMsg -> CommandMsg -> IO ()
handler f mv cmd = do putMVar mv cmd >>
                        commandEventCreate wxEVT_COMMAND_MENU_SELECTED ficsEventId >>=
                        \ev -> evtHandlerAddPendingEvent f ev


registerFicsEvents :: Frame () -> IO () -> IO ()
registerFicsEvents f action = evtHandlerOnMenuCommand f ficsEventId action

action :: Handle -> MVar CommandMsg -> TextCtrl () -> ListCtrl() -> Panel () -> Var (Position) -> IO ()
action h mvar ct sl b bVar = takeMVar mvar >>= \cmd -> case cmd of
                      SoughtMsg _ msg -> updateSeekList sl msg >> appendText ct (BS.unpack msg ++ "\n") >> return ()
                      ObserveMsg _ pos -> updateBoard b bVar pos
                      GamesMsg _ msg -> appendText ct (BS.unpack msg ++ "\n") >> return ()
                      PositionMessage pos -> updateBoard b bVar pos
                      TextMessage text -> appendText ct (BS.unpack text ++ "\n") >> return ()
                      LoginMessage     -> hPutStrLn h "Schoon"
                      PasswordMessage  -> hPutStrLn h "efgeon"
                      LoggedInMessage  -> hPutStrLn h "iset block 1"
                      _                -> return ()



updateBoard :: Panel () -> Var (Position) -> Position -> IO ()
updateBoard p boardVar position = case reverse position of
                                 [] -> return ()
                                 pos -> set boardVar [value := pos] >> repaint p


updateSeekList :: ListCtrl() -> BS.ByteString -> IO ()
updateSeekList l msg = case seeks of
                            [] -> return ()
                            (xs) -> do set l [items := [[handle, show rating, show gt] | (Seek _ rating handle gt _ _ _ _ _) <- seeks]]
                         where
                            seeks = parseSeeks msg

onSoughtListEvent list eventList = case eventList of
      ListItemSelected idx    -> do
                                  listCtrlGetItemText list idx >>= logMessage . (++) "item selected: "
                                  set list [items :=
                                    [[handle, show rating, show gt] | (Seek _ rating handle gt _ _ _ _ _) <- []]]

      ListItemDeselected idx  -> logMessage ("item de-selected: " ++ show idx)
      other                   -> logMessage ("list control event.")


emitCommand :: TextCtrl () -> Handle -> IO ()
emitCommand e h = get e text >>= \command ->
                  set e [text := ""] >>
                  hPutStrLn h command

