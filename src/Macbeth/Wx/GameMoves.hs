module Macbeth.Wx.GameMoves (
  wxGameMoves,
  main
) where

import Macbeth.Api.Api
import Macbeth.Api.Move
import Macbeth.Wx.Utils

import Control.Applicative
import Control.Concurrent.STM
import Data.Maybe
import Graphics.UI.WX
import Graphics.UI.WXCore

main = start $ do
  f <- frame []
  t <- newTVarIO []
  (p, u) <- wxGameMoves f t
  set f [layout := widget p]


data MoveShort = MoveShort {num :: Int, turnColor :: PColor, move :: String}



moveShort :: Panel () -> Int -> PColor -> String -> MoveShort
moveShort p num turnColor move = MoveShort num turnColor move

convertM p m = moveShort p (moveNumber m) (turn m) (fromJust $ movePretty m)

wxGameMoves :: Frame () -> TVar [Move] -> IO (Panel (), IO ())
wxGameMoves f t = do
  p <- panel f []
  txtCtrls <- sequence $ replicate 180 (staticTextSmall p "")

--  selected <- readTVarIO vSel
--  vSel <- newTVarIO 0
--  set (txtCtrl $ mx' !! selected) [bgcolor := red]
--  set p [on rightKey := doRightKey vSel mx' ]
--  set p [on leftKey := doLeftKey vSel mx' ]

  let update = do
            moves <- readTVarIO t
            let mx = fmap (convertM p) moves
            let labels = layoutMoves mx
            sequence_ $ zipWith (\tCtrl mv -> set tCtrl [text := mv, visible := True]) txtCtrls labels
            set p [layout := margin 15 $ grid 10 0 (layoutSt $ take (length labels) txtCtrls)]
            repaint p
  update
  return (p, update)


{-doRightKey vSel mx = do
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

layoutSt (t1:t2:t3:tx) = [[hfill $ widget t1, hfill $ widget t2, hfill $ widget t3]] ++ (layoutSt tx)
layoutSt _ = []

layoutMoves :: [MoveShort] -> [String]
layoutMoves [] = []
layoutMoves [s1]
  | turnColor s1 == White = [show $ num s1, "...", move s1]
  | otherwise = [show $ num s1, move s1, ""]

layoutMoves (s1:s2:sx)
  | turnColor s1 == White = [show $ num s1, "...", move s1] ++ (layoutMoves $ s2:sx)
  | otherwise = [show $ num s1, move s1, move s2] ++ (layoutMoves sx)

staticTextSmall :: Panel () -> String -> IO (StaticText ())
staticTextSmall p s = staticText p [ text := s
                                       , fontFace := "Avenir Next Medium"
                                       , fontSize := 14
                                       , visible := False
                                       , fontWeight := WeightNormal]



