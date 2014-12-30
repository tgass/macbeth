module Utils (
  formatTime
) where


import Control.Monad.State

formatTime :: Int -> String
formatTime seconds = show h ++ " : " ++ format m ++ " : " ++ format s
  where
    (_, (h,m,s)) = runState ( return seconds >>= calc ) (0,0,0)


calc :: Int -> State (Int, Int, Int) Int
calc seconds
  | seconds >= 3600 = get >>= \(h, m, s) -> put (h+1, m, s) >> calc (seconds - 3600)
  | seconds >= 60 = get >>= \(h, m, s) -> put (h, m+1, s) >> calc (seconds - 60)
  | otherwise = get >>= \(h, m, s) -> put (h, m, seconds) >> return 0

format :: Int -> String
format i
  | i < 10 = "0" ++ show i
  | otherwise = show i
