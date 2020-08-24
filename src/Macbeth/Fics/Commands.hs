module Macbeth.Fics.Commands where

import           Data.Maybe
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Seek (SeekColor)
import qualified Macbeth.Fics.Api.Seek as Seek
import           Macbeth.Fics.Api.GameType
import           System.IO

abort :: Handle -> IO ()
abort h = hPutStrLn h "6 abort" 

accept :: Handle -> IO ()
accept h = hPutStrLn h "6 accept" 

acceptId :: Handle -> Int -> IO ()
acceptId h offerId = hPutStrLn h $ "6 accept " ++ show offerId

decline :: Handle -> IO ()
decline h = hPutStrLn h "6 decline" 

declineId :: Handle -> Int -> IO ()
declineId h offerId = hPutStrLn h $ "6 decline " ++ show offerId

draw :: Handle -> IO ()
draw h = hPutStrLn h "6 draw" 

finger :: Handle -> Maybe String -> IO ()
finger h mUser = hPutStrLn h $ "6 finger " ++ fromMaybe "" mUser

games :: Handle -> IO ()
games h = hPutStrLn h "6 games"

history :: Handle -> Maybe String -> IO ()
history h mUser = hPutStrLn h $ "6 history " ++ fromMaybe "" mUser

observe :: Handle -> String -> IO ()
observe h user = hPutStrLn h $ "6 observe " ++ user

observeGame :: Handle -> Int -> IO ()
observeGame h gameId = hPutStrLn h $ "6 observe " ++ show gameId

partner :: Handle -> String -> IO ()
partner h user = hPutStrLn h $ "6 partner " ++ user

play :: Handle -> String -> IO ()
play h user = hPutStrLn h $ "6 play " ++ user

resign :: Handle -> IO ()
resign h = hPutStrLn h "6 resign" 

takeback :: Handle -> Int -> IO ()
takeback h halfmoves = hPutStrLn h $ "7 takeback " ++ show halfmoves 

tell :: Handle -> String -> String -> IO ()
tell h user msg = hPutStrLn h $ "7 tell " ++ user ++ " " ++ msg

unobserve :: Handle -> GameId -> IO ()
unobserve h gameId = hPutStrLn h $ "5 unobserve " ++ show gameId

match :: Handle -> String -> IO ()
match h user = hPutStrLn h $ "6 match " ++ user

match2 :: Handle -> String -> Bool -> Int -> Int -> SeekColor -> Category -> Maybe WildBoard -> IO ()
match2  h user rated time inc color cat mWild = hPutStrLn h $ ("4 match " ++) $ unwords $ filter (/="") [
       user
     , Seek.convertIsRated rated
     , show time
     , show inc
     , Seek.convertColor color
     , gameTypeSelectionToString cat mWild
     ]

-- seek [scTime scInc] [scRated|unscRated] [white|black] [crazyhouse] [suicide]
--      [wild #] [auto|scManual] [formula] [rating-range]
seek :: Handle -> Seek.SeekConfig -> IO ()
seek h = hPutStrLn h . ("4 " ++) . Seek.mkSeekString 

ping :: Handle -> IO ()
ping h = hPutStrLn h "5 ping"

promote :: Handle -> PType -> IO ()
promote h ptype = hPutStrLn h $ "5 promote " ++ show ptype

who :: Handle -> IO ()
who h = hPutStrLn h "5 who"

withdrawId :: Handle -> Int -> IO ()
withdrawId h offerId = hPutStrLn h $ "6 withdraw " ++ show offerId


