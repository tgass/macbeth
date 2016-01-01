module Macbeth.Wx.GameMoves (
  wxGameMoves
) where

import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move

import Control.Applicative hiding (empty)
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe
import Graphics.UI.WX
import Graphics.UI.WXCore

data MoveShort = MoveShort {num :: Int, turnColor :: PColor, move :: String}

moveShort :: Move -> MoveShort
moveShort m = MoveShort (moveNumber m) (turn m) (fromJust $ movePretty m)

wxGameMoves :: SplitterWindow () -> TVar [Move] -> IO (ScrolledWindow (), IO ())
wxGameMoves f t = do
  sw     <- scrolledWindow f [ scrollRate := sz 5 5]
  p <- panel sw []
  txtCtrls <- replicateM 250 (staticTextSmall p "")
  let update = do
            moves <- readTVarIO t
            let mx = reverse $ moveShort <$> filter (isJust . movePretty) moves
            let labels = layoutMoves mx
            zipWithM_ (\tCtrl mv -> set tCtrl [text := mv, visible := True]) txtCtrls labels
            sizerX <- sizerFromLayout sw  (if null mx then empty
                                           else container p $ margin 30 $  grid 5 0 (layoutSt $ take (length labels) txtCtrls))
            windowSetSizer sw sizerX
            windowFitInside sw

  update
  return (sw, update)

layoutSt (t1:t2:t3:tx) = [ vspace 18
                         , widget t1
                         , widget t2
                         , widget t3] : layoutSt tx
layoutSt _ = []


layoutMoves :: [MoveShort] -> [String]
layoutMoves [] = []
layoutMoves [s1]
  | turnColor s1 == White = [show (num s1 - 1) ++ ".", "...", move s1]
  | otherwise = [show (num s1) ++ ".", move s1, ""]
layoutMoves (s1:s2:sx)
  | turnColor s1 == White = [show (num s1 - 1) ++ ".", "...", move s1] ++ layoutMoves (s2:sx)
  | otherwise = [show (num s1) ++ ".", move s1, move s2] ++ layoutMoves sx



staticTextSmall :: Panel () -> String -> IO (StaticText ())
staticTextSmall p s = staticText p [ text := s
                                       , fontFace := "Avenir Next Medium"
                                       , fontSize := 14
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


