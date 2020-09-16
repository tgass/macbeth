module Macbeth.Fics.Commands where

import           Data.Monoid
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Seek (SeekColor)
import           Macbeth.Fics.Api.GameType
import           Macbeth.Fics.Commands.Seek

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
  | Stored
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
  show Stored                = "stored"
  show Ping                  = "ping"
  show (Promote ptype)       = "promote " <> show ptype
  show Who                   = "who"
  show (Withdraw reqid)      =" withdraw " <> show reqid

mkMatch :: String -> Bool -> Int -> Int -> SeekColor -> Category -> Maybe WildBoard -> String
mkMatch user rated time inc color cat mWild = ("match " <>) $ unwords $ filter (/="") [
    user
  , convertIsRated rated
  , show time
  , show inc
  , convertColor color
  , gameTypeSelectionToString cat mWild
  ]
