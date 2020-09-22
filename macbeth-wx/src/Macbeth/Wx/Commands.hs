module Macbeth.Wx.Commands where

import           Control.Exception
import           Data.Monoid
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Seek (SeekColor)
import           Macbeth.Fics.Api.GameType
import           Macbeth.Fics.Commands
import           Macbeth.Fics.Commands.Seek
import           System.IO

abort :: Handle -> IO ()
abort h = command h Abort

accept :: Handle -> IO ()
accept h = command h Accept

acceptId :: Handle -> Int -> IO ()
acceptId h offerId = command h $ AcceptId offerId

adjourn :: Handle -> IO ()
adjourn h = command h Adjourn

decline :: Handle -> IO ()
decline h = command h Decline

declineId :: Handle -> Int -> IO ()
declineId h offerId = command h $ DeclineId offerId

draw :: Handle -> IO ()
draw h = command h Draw

finger :: Handle -> Maybe String -> IO ()
finger h mUser = command h $ Finger mUser

games :: Handle -> IO ()
games h = command h Games

history :: Handle -> Maybe String -> IO ()
history h mUser = command h $ History mUser

observe :: Handle -> String -> IO ()
observe h user = command h $ Observe user

observeGame :: Handle -> Int -> IO ()
observeGame h gameId = command h $ ObserveGame gameId

partner :: Handle -> String -> IO ()
partner h user = command h $ Partner user

play :: Handle -> String -> IO ()
play h user = command h $ Play user

resign :: Handle -> IO ()
resign h = command h Resign

takeback :: Handle -> Int -> IO ()
takeback h halfmoves = command h $ Takeback halfmoves

tell :: Handle -> String -> String -> IO ()
tell h user msg = command h $ Tell user msg

unobserve :: Handle -> GameId -> IO ()
unobserve h gameId = command h $ Unobserve gameId

match :: Handle -> String -> IO ()
match h user = command h $ Match user

match2 :: Handle -> String -> Bool -> Int -> Int -> SeekColor -> Category -> Maybe WildBoard -> IO ()
match2 h user rated time inc color cat mWild = command h $ Match2 user rated time inc color cat mWild

seek :: Handle -> SeekConfig -> IO ()
seek h config = command h $ Seek config

stored :: Handle -> IO ()
stored h = command h Stored

ping :: Handle -> IO ()
ping h = command h Ping

promote :: Handle -> PType -> IO ()
promote h ptype = command h $ Promote ptype

who :: Handle -> IO ()
who h = command h Who

withdrawId :: Handle -> Int -> IO ()
withdrawId h offerId = command h $ Withdraw offerId


command :: Handle -> Command -> IO ()
command h command = catch (hPutStrLn h $ "6 " ++ show command) $ \(e :: SomeException) -> return ()
