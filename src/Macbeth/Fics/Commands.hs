module Macbeth.Fics.Commands where

import           Data.Maybe
import           Macbeth.Fics.Api.Api
import qualified Macbeth.Wx.Config.SeekConfig as SC
import           Macbeth.Wx.GameType
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

match2 :: Handle -> String -> Bool -> Int -> Int -> SC.SeekColor -> Category -> Maybe WildBoard -> IO ()
match2  h user rated time inc color cat mWild = hPutStrLn h $ ("4 match " ++) $ unwords $ filter (/="") [
       user
     , convertIsRated rated
     , show time
     , show inc
     , convertColor color
     , gameTypeSelectionToString cat mWild
     ]

-- seek [scTime scInc] [scRated|unscRated] [white|black] [crazyhouse] [suicide]
--      [wild #] [auto|scManual] [formula] [rating-range]
seek :: Handle -> Int -> Int -> Bool -> SC.SeekColor -> Category -> Maybe WildBoard -> Bool -> Int -> Int -> IO ()
seek h time inc rated color cat mWild manual ratingFrom ratingTo = hPutStrLn h $ ("4 seek " ++) $ unwords $ filter (/= "") [
    show time
  , show inc
  , convertIsRated rated
  , convertColor color
  , gameTypeSelectionToString cat mWild
  , convertIsManual manual
  , convertRatingRange ratingFrom ratingTo
  ]

ping :: Handle -> IO ()
ping h = hPutStrLn h "5 ping"

promote :: Handle -> PType -> IO ()
promote h ptype = hPutStrLn h $ "5 promote " ++ show ptype

who :: Handle -> IO ()
who h = hPutStrLn h "5 who"

withdrawId :: Handle -> Int -> IO ()
withdrawId h offerId = hPutStrLn h $ "6 withdraw " ++ show offerId


convertIsRated :: Bool -> String
convertIsRated True = "r"
convertIsRated False = "u"

convertColor :: SC.SeekColor -> String
convertColor SC.White = "w"
convertColor SC.Black = "b"
convertColor SC.Automatic = ""

convertRatingRange :: Int -> Int -> String
convertRatingRange from to = show from ++ "-" ++ show to

convertIsManual :: Bool -> String
convertIsManual True = "m"
convertIsManual False = "a"


