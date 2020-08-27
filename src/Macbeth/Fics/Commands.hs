module Macbeth.Fics.Commands where

import           Data.Monoid
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Seek (SeekColor)
import           Macbeth.Fics.Api.GameType
import           Macbeth.Fics.Commands.Seek
import           System.IO

type Username = String

data Command =
    Abort 
  | Accept
  | AcceptId Int
  | Decline
  | DeclineId Int
  | Draw
  | Finger (Maybe String)
  | Games
  | History (Maybe String)
  | Observe Username
  | ObserveGame Int
  | Partner Username
  | Play Username
  | Resign
  | Takeback Int
  | Tell Username String
  | Unobserve GameId
  | Match Username
  | Match2 String Bool Int Int SeekColor Category (Maybe WildBoard)
  | Seek SeekConfig
  | Ping
  | Promote PType
  | Who 
  | Withdraw Int 

instance Show Command where
  show Abort                 = "abort"
  show Accept                = "accept"
  show (AcceptId reqid)      = "accept " <> show reqid
  show Decline               = "decline"
  show (DeclineId reqid)     = "decline " <> show reqid
  show Draw                  = "draw"
  show (Finger (Just user))  = "finger " <> user
  show (Finger Nothing)      = "finger"
  show Games                 = "games"
  show (History (Just user)) = "history " <> user
  show (History Nothing)     = "history"
  show (Observe user)        = "observe " <> user
  show (ObserveGame gameId)  = "observe " <> show gameId
  show (Partner user)        = "partner " <> user
  show (Play user)           = "play " <> user
  show Resign                = "resign"
  show (Takeback num)        = "takeback " <> show num
  show (Tell user msg)       = "tell " <> user <> " " <> msg
  show (Unobserve gameId)    = "unobserve " <> show gameId
  show (Match user)          = "match " <> user
  show (Match2 user rated time inc color cat mWild) = mkMatch user rated time inc color cat mWild
  show (Seek config)         = mkSeekString config
  show Ping                  = "ping"
  show (Promote ptype)       = "promote " <> show ptype
  show Who                   = "who"
  show (Withdraw reqid)      =" withdraw " <> show reqid

abort :: Handle -> IO ()
abort h = command h Abort

accept :: Handle -> IO ()
accept h = command h Accept

acceptId :: Handle -> Int -> IO ()
acceptId h offerId = command h $ AcceptId offerId

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

ping :: Handle -> IO ()
ping h = command h Ping

promote :: Handle -> PType -> IO ()
promote h ptype = command h $ Promote ptype

who :: Handle -> IO ()
who h = command h Who

withdrawId :: Handle -> Int -> IO ()
withdrawId h offerId = command h $ Withdraw offerId


mkMatch :: String -> Bool -> Int -> Int -> SeekColor -> Category -> Maybe WildBoard -> String
mkMatch user rated time inc color cat mWild = ("match " <>) $ unwords $ filter (/="") [
    user
  , convertIsRated rated
  , show time
  , show inc
  , convertColor color
  , gameTypeSelectionToString cat mWild
  ]

command :: Handle -> Command -> IO ()
command h c = hPutStrLn h $ "6 " ++ show c


