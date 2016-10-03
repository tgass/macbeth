module Macbeth.Utils.Utils (
    formatTime
  , encrypt
  , decrypt
  , bind2
) where

import Macbeth.Utils.Salt

import Control.Monad
import Control.Monad.State
import Crypto.Cipher
import qualified Data.ByteString.Char8 as B


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


aes128 :: AES128
aes128 = either (error . show) cipherInit $ makeKey (B.pack salt)


encrypt :: String -> String
encrypt = B.unpack . ctrCombine aes128 nullIV . B.pack


decrypt :: String -> String
decrypt = encrypt


-- taken from Prelude.Generalize
(.::) :: (b -> c) -> (a -> a1 -> a2 -> b) -> a -> a1 -> a2 -> c
(.::) = (.) . (.) . (.)


-- taken from Prelude.Generalize
bind2 :: Monad m => (x -> y -> m a) -> m x -> m y -> m a
bind2 = join .:: liftM2

