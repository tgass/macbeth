module Macbeth.Fics.Commands where

import           Control.Monad.Catch
import           Control.Monad.Reader
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
  | Adjourn
  | Decline
  | DeclineId Int
  | Draw
  | Finger (Maybe String)
  | Games
  | History (Maybe String)
  | Kibitz GameId String
  | Observe Username
  | ObserveGame Int
  | Partner Username
  | Play Username
  | Resign
  | Takeback Int
  | Tell Username String
  | TellChannel ChannelId String
  | Say String
  | Unobserve GameId
  | Match Username
  | Match2 String Bool Int Int SeekColor Category (Maybe WildBoard)
  | Seek SeekConfig
  | Stored
  | Ping
  | Promote PType
  | Who 
  | Withdraw Int 
  | Whisper GameId String

class HasHandle a where
  getHandle :: a -> Handle
  getCommandId :: a -> IO Int

instance Show Command where
  show Abort                             = "abort"
  show Accept                            = "accept"
  show (AcceptId reqid)                  = "accept " <> show reqid
  show Adjourn                           = "adjourn"
  show Decline                           = "decline"
  show (DeclineId reqid)                 = "decline " <> show reqid
  show Draw                              = "draw"
  show (Finger (Just user))              = "finger " <> user
  show (Finger Nothing)                  = "finger"
  show Games                             = "games"
  show (History (Just user))             = "history " <> user
  show (History Nothing)                 = "history"
  show (Kibitz (GameId gid) msg)         = "xkibitz " <> show gid <> " " <> msg
  show (Observe user)                    = "observe " <> user
  show (ObserveGame gameId)              = "observe " <> show gameId
  show (Partner user)                    = "partner " <> user
  show (Play user)                       = "play " <> user
  show Resign                            = "resign"
  show (Takeback num)                    = "takeback " <> show num
  show (Tell user msg)                   = "tell " <> user <> " " <> msg
  show (TellChannel (ChannelId cid) msg) = "tell " <> show cid <> " " <> msg
  show (Say msg)                         = "say " <> msg
  show (Unobserve gameId)                = "unobserve " <> show gameId
  show (Match user)                      = "match " <> user
  show (Match2 user rated time inc color cat mWild) 
                                         = mkMatch user rated time inc color cat mWild
  show (Seek config)                     = mkSeekString config
  show Stored                            = "stored"
  show Ping                              = "ping"
  show (Promote ptype)                   = "promote " <> show ptype
  show Who                               = "who"
  show (Withdraw reqid)                  =" withdraw " <> show reqid
  show (Whisper (GameId gid) msg)        = "xwhisper " <> show gid <> " " <> msg

mkMatch :: String -> Bool -> Int -> Int -> SeekColor -> Category -> Maybe WildBoard -> String
mkMatch user rated time inc color cat mWild = ("match " <>) $ unwords $ filter (/="") [
    user
  , convertIsRated rated
  , show time
  , show inc
  , convertColor color
  , gameTypeSelectionToString cat mWild
  ]

abort :: HasHandle a => a  -> IO ()
abort env = run env Abort

accept :: HasHandle a => a  -> IO ()
accept env = run env Accept

acceptId :: HasHandle a => a  -> Int -> IO ()
acceptId env offerId = run env $ AcceptId offerId

adjourn :: HasHandle a => a  -> IO ()
adjourn env = run env Adjourn

decline :: HasHandle a => a  -> IO ()
decline env = run env Decline

declineId :: HasHandle a => a  -> Int -> IO ()
declineId env offerId = run env $ DeclineId offerId

draw :: HasHandle a => a  -> IO ()
draw env = run env Draw

finger :: HasHandle a => a -> Maybe String -> IO ()
finger env mUser = run env $ Finger mUser

games :: HasHandle a => a -> IO ()
games env = run env Games

history :: HasHandle a => a -> Maybe String -> IO ()
history env mUser = run env $ History mUser

kibitz :: HasHandle a => a  -> GameId -> String -> IO ()
kibitz env gameId msg = run env $ Kibitz gameId msg

observe :: HasHandle a => a  -> String -> IO ()
observe env user = run env $ Observe user

observeGame :: HasHandle a => a  -> Int -> IO ()
observeGame env gameId = run env $ ObserveGame gameId

partner :: HasHandle a => a  -> String -> IO ()
partner env user = run env $ Partner user

play :: HasHandle a => a  -> String -> IO ()
play env user = run env $ Play user

resign :: HasHandle a => a  -> IO ()
resign env = run env Resign

takeback :: HasHandle a => a -> Int -> IO ()
takeback env halfmoves = run env $ Takeback halfmoves

tell :: HasHandle a => a  -> String -> String -> IO ()
tell env user msg = run env $ Tell user msg

tellChannel :: HasHandle a => a  -> ChannelId -> String -> IO ()
tellChannel env cid msg = run env $ TellChannel cid msg

say :: HasHandle a => a  -> String -> IO ()
say env msg = run env $ Say msg

unobserve :: HasHandle a => a  -> GameId -> IO ()
unobserve env gameId = run env $ Unobserve gameId

match :: HasHandle a => a  -> String -> IO ()
match env user = run env $ Match user

match2 :: HasHandle a => a  -> String -> Bool -> Int -> Int -> SeekColor -> Category -> Maybe WildBoard -> IO ()
match2 env user rated time inc color cat mWild = run env $ Match2 user rated time inc color cat mWild

seek :: HasHandle a => a  -> SeekConfig -> IO ()
seek env config = run env $ Seek config

stored :: HasHandle a => a  -> IO ()
stored env = run env Stored

ping :: HasHandle a => a -> IO ()
ping env = run env Ping

promote :: HasHandle a => a  -> PType -> IO ()
promote env ptype = run env $ Promote ptype

who :: HasHandle a => a  -> IO ()
who env = run env Who

withdrawId :: HasHandle a => a  -> Int -> IO ()
withdrawId env offerId = run env $ Withdraw offerId

whisper :: HasHandle a => a  -> GameId -> String -> IO ()
whisper env gameId msg = run env $ Whisper gameId msg

message :: HasHandle a => a -> String -> IO ()
message env cmd = flip runReaderT env $ do
  h <- asks getHandle
  liftIO $ hPutStrLn h cmd

messageWithCommandId :: HasHandle a => a -> String -> IO ()
messageWithCommandId env cmd = flip runReaderT env $ do
  h <- asks getHandle
  cid <- liftIO =<< asks getCommandId
  liftIO $ hPutStrLn h $ show cid ++ " " ++ cmd

run :: (MonadCatch m, MonadIO m, HasHandle a) => a -> Command -> m ()
run env command = flip runReaderT env $ do
  h <- asks getHandle
  cid <- liftIO =<< asks getCommandId
  catch (liftIO $ hPutStrLn h $ show cid ++ " " ++ show command) $ \(_ :: SomeException) -> return ()

