{-# LANGUAGE OverloadedStrings #-}

module Main where

import Board
import FicsConnection
import Seek

import Control.Concurrent (newEmptyMVar)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO (Handle, hPutStrLn)

main = start gui

gui = do
    -- main frame
    f  <- frame []
    mv <- newEmptyMVar
    h <- getFicsHandle f mv

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
    sl  <- listCtrl slp [columns := [("handle", AlignLeft, -1), ("rating", AlignRight, -1), ("game type", AlignRight, -1)]]


    -- tab2 : console
    cp <- panel nb []
    ct <- textCtrlEx cp (wxTE_MULTILINE .+. wxTE_RICH) [font := fontFixed]
    ce <- entry cp []
    set ce [on enterKey := emitCommand ce h]

    -- register event handling
    registerFicsEvents f h mv [updateConsole ct, updateSeekList sl, updateBoard left boardVar]

    set f [layout := container s $ fill $ vsplit s 15 400
                      (container left $ space 320 320)
                      (container right $
                         column 0
                         [ tabs nb
                            [ tab "Sought list" $ container slp $ fill $ widget sl
                            , tab "Console" $ container cp $ (column 5 [
                                                 floatLeft $ expand $ hstretch $ widget ct
                                                ,expand $ hstretch $ widget ce]) ]
                         ]
                       )
          , clientSize := sz 600 320
          ]
    return ()

updateBoard :: Panel () -> Var(Position) -> [BS.ByteString] -> IO ()
updateBoard p boardVar lines = case reverse positions of
                                 [] -> return ()
                                 (Move(pos):ms) -> set boardVar [value := pos] >> repaint p
                               where
                                  positions = catMaybes $ fmap parseMove lines

updateConsole :: TextCtrl () -> [BS.ByteString] -> IO ()
updateConsole t lines = appendText t (BS.unpack $ BS.unlines $ filter (/= "fics% ") lines)

updateSeekList :: ListCtrl() -> [BS.ByteString] -> IO ()
updateSeekList l lines = case seeks of
                            [] -> return ()
                            (xs) -> do set l [items := [[handle, show rating, show gt] | (Seek _ rating handle gt _ _ _ _ _) <- seeks]]
                         where
                            seeks = catMaybes $ fmap parseSeek lines

emitCommand :: TextCtrl () -> Handle -> IO ()
emitCommand e h = get e text >>= \command ->
                  set e [text := ""] >>
                  hPutStrLn h command
