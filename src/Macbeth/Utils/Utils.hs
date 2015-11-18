{-# LANGUAGE DefaultSignatures #-}
module Macbeth.Utils.Utils (
  formatTime,
  ToBitMask(..),
  fromBitMask,
  isInBitMask
) where

import Data.Bits
import Control.Monad
import Control.Monad.State

formatTime :: Int -> String
formatTime seconds = show h ++ " : " ++ format m ++ " : " ++ format s
  where
    (_, (h,m,s)) = runState (calc seconds) (0,0,0)


calc :: Int -> State (Int, Int, Int) Int
calc seconds
  | seconds >= 3600 = get >>= \(h, m, s) -> put (h+1, m, s) >> calc (seconds - 3600)
  | seconds >= 60 = get >>= \(h, m, s) -> put (h, m+1, s) >> calc (seconds - 60)
  | otherwise = get >>= \(h, m, _) -> put (h, m, max 0 seconds) >> return 0

format :: Int -> String
format i
  | i < 10 = "0" ++ show i
  | otherwise = show i


{-
  Thank you Nikita Volkov
  http://stackoverflow.com/questions/15910363/represent-a-list-of-enums-bitwise-as-an-int
-}
class ToBitMask a where
  toBitMask :: a -> Int
  -- | Using a DefaultSignatures extension to declare a default signature with
  -- an `Enum` constraint without affecting the constraints of the class itself.
  default toBitMask :: Enum a => a -> Int
  toBitMask = shiftL 1 . fromEnum

instance ( ToBitMask a ) => ToBitMask [a] where
  toBitMask = foldr (.|.) 0 . map toBitMask

-- | Not making this a typeclass, since it already generalizes over all
-- imaginable instances with help of `MonadPlus`.
fromBitMask ::
  ( MonadPlus m, Enum a, Bounded a, ToBitMask a ) =>
    Int -> m a
fromBitMask bm = msum $ map asInBM $ enumFrom minBound where
  asInBM a = if isInBitMask bm a then return a else mzero

isInBitMask :: ( ToBitMask a ) => Int -> a -> Bool
isInBitMask bm a = let aBM = toBitMask a in aBM == aBM .&. bm

