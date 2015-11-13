module Macbeth.Wx.GameMoves (
  wxGameMoves
) where

import Macbeth.Api.Api
import Macbeth.Api.Move

import Control.Concurrent.STM
import Control.Monad
import Data.Maybe
import Graphics.UI.WX

data MoveShort = MoveShort {num :: Int, turnColor :: PColor, move :: String}

moveShort :: Move -> MoveShort
moveShort m = MoveShort (moveNumber m) (turn m) (fromJust $ movePretty m)

wxGameMoves :: SplitterWindow () -> TVar [Move] -> IO (ScrolledWindow (), IO ())
wxGameMoves f t = do
  scrl     <- scrolledWindow f [scrollRate := sz 10 10, fullRepaintOnResize := True]
  p <- panel scrl []
  txtCtrls <- replicateM 250 (staticTextSmall p "")
  let update = do
            moves <- readTVarIO t
            let mx = reverse $ fmap moveShort $ filter (isJust . movePretty) moves
            let labels = layoutMoves mx
            zipWithM_ (\tCtrl mv -> set tCtrl [text := mv, visible := True]) txtCtrls labels
            set scrl [layout := container p $ margin 30 $  marginTop $
              if null mx then empty else grid 5 0 (layoutSt $ take (length labels) txtCtrls)]
            repaint p
  update
  return (scrl, update)

layoutSt (t1:t2:t3:tx) = [widget t1, widget t2, widget t3] : layoutSt tx
layoutSt _ = []


layoutMoves :: [MoveShort] -> [String]
layoutMoves [] = []
layoutMoves [s1]
  | turnColor s1 == White = [show (num s1) ++ ".", "...", move s1]
  | otherwise = [show (num s1) ++ ".", move s1, ""]
layoutMoves (s1:s2:sx)
  | turnColor s1 == White = [show (num s1) ++ ".", "...", move s1] ++ layoutMoves (s2:sx)
  | otherwise = [show (num s1) ++ ".", move s1, move s2] ++ layoutMoves sx



staticTextSmall :: Panel () -> String -> IO (StaticText ())
staticTextSmall p s = staticText p [ text := s
                                       , fontFace := "Avenir Next Medium"
                                       , fontSize := 12
                                       , visible := False
                                       , fontWeight := WeightNormal]

{-

--  selected <- readTVarIO vSel
--  vSel <- newTVarIO 0
--  set (txtCtrl $ mx' !! selected) [bgcolor := red]
--  set p [on rightKey := doRightKey vSel mx' ]
--  set p [on leftKey := doLeftKey vSel mx' ]

doRightKey vSel mx = do
  selected <- readTVarIO vSel
  set (txtCtrl $ mx !! selected) [bgcolor := colorSystem ColorBackground]
  selected <- atomically $ modifyTVar vSel (\x -> min (length mx - 1) (x + 1)) >> readTVar vSel
  set (txtCtrl $ mx !! selected) [bgcolor := red]

doLeftKey vSel mx = do
  selected <- readTVarIO vSel
  set (txtCtrl $ mx !! selected) [bgcolor := colorSystem ColorBackground]
  selected <- atomically $ modifyTVar vSel (\x -> max 0 (x - 1)) >> readTVar vSel
  set (txtCtrl $ mx !! selected) [bgcolor := red]
-}


