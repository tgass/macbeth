module Macbeth.Fics.Connection (
  connection,
  timesealConnection,
  linesC,
  parseC,
  blockC,
  unblockC,
  crop,
  lines',
  readId
) where


import           Control.Concurrent (forkFinally)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.List hiding (filter)
import           Data.Conduit.Process
import           Data.Maybe
import           GHC.Show (showLitString)
import           Macbeth.Fics.Message hiding (gameId)
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Game
import           Macbeth.Fics.Parsers.MessageParser
import qualified Macbeth.Fics.Api.Move as Move
import qualified Macbeth.Fics.Api.Result as R
import           Macbeth.Fics.Timeseal
import           System.Log.Logger
import           Network
import           Prelude hiding (log)
import           System.IO


logger :: String
logger = "Macbeth.Fics.Connection"


data HelperState = HelperState { takebackAccptedBy :: Maybe (Maybe String)
                               , illegalMove :: Maybe String
                               , newGameUserParams :: Maybe GameParams }


connection :: IO (Handle, Chan Message)
connection = runResourceT $ do
   (_, h) <- allocate (connectTo "freechess.org" $ PortNumber 5000) hClose
   liftIO $ hSetBuffering h LineBuffering
   chan <- liftIO newChan
   void $ resourceForkIO $ liftIO $ chain (sourceHandle h) chan
   return (h, chan)


timesealConnection :: TimesealEnv a => a -> IO (Handle, Chan Message)
timesealConnection timesealEnv = do
  zsealExec <- getTimesealExec timesealEnv
  chan <- newChan
  handleVar <- newEmptyMVar
  void $ flip forkFinally (\e -> writeChan chan (ConnectionClosed $ show e) >> infoM logger "Exiting zseal process ...") $ 
    withCheckedProcessCleanup (proc zsealExec []) $ \hsock fromProcess ClosedStream -> do
      hSetBuffering hsock LineBuffering
      putMVar handleVar hsock
      chain fromProcess chan
  h <- readMVar handleVar
  return (h, chan)

chain :: Source IO BS.ByteString -> Chan Message -> IO ()
chain src chan = flip evalStateT emptyState $ transPipe lift 
  src $$ 
  linesC =$
  blockC BS.empty =$
  logStreamC =$
  unblockC =$
  parseC =$
  stateC =$
  copyC chan =$
  logMessageC =$
  sink chan


emptyState :: HelperState
emptyState = HelperState Nothing Nothing Nothing


sink :: MonadIO m => Chan Message -> Sink Message m ()
sink chan = awaitForever $ liftIO . writeChan chan


logMessageC :: MonadIO m => Conduit Message m Message
logMessageC = awaitForever $ \cmd -> case cmd of
  NewSeek{} -> yield cmd
  RemoveSeeks{} -> yield cmd
  _ -> liftIO (infoM logger $ show cmd) >> yield cmd


copyC :: MonadIO m => Chan Message -> Conduit Message m Message
copyC chan = awaitForever $ \case
  m@(NewGameUser gameId' gameParams') -> do
    chan' <- liftIO $ dupChan chan
    sourceList [m, WxOpenBoard gameId' gameParams' chan']
  m@(Observing gameId' gameParams') -> do
    chan' <- liftIO $ dupChan chan
    sourceList [m, WxOpenBoard gameId' gameParams' chan']
  cmd -> yield cmd


stateC :: Conduit Message (StateT HelperState IO) Message
stateC = awaitForever $ \cmd -> do
  state' <- get
  case cmd of

    NewGameParamsUser params -> put (state' {newGameUserParams = Just params}) >> sourceNull

    NewGameIdUser gameId'
      | isJust $ newGameUserParams state' -> do
          put $ state' {newGameUserParams = Nothing}
          sourceList [ NewGameUser gameId' $ fromJust $ newGameUserParams state']
      | otherwise -> sourceList [ TextMessage "*** Macbeth ***: Could not find gameParams." ]

    TakebackAccepted username -> put (state' {takebackAccptedBy = Just username}) >> sourceNull

    IllegalMove m -> put (state' {illegalMove = Just m}) >> sourceNull

    m@(GameMove _ move')
      | Move.isCheckmate move' && Move.isOponentMove move' -> sourceList $ cmd : [convertMoveToResult move']
      | isJust $ takebackAccptedBy state' -> do
          put $ state' {takebackAccptedBy = Nothing}
          sourceList [m{context = Move.Takeback $ fromJust $ takebackAccptedBy state'}]
      | isJust $ illegalMove state' -> do
          put $ state' {illegalMove = Nothing}
          sourceList [m{context = Move.Illegal $ fromJust $ illegalMove state'}]
      | otherwise -> sourceList [m]

    _ -> yield cmd


parseC :: Monad m => Conduit BS.ByteString m Message
parseC = awaitForever $ \str -> case parseMessage str of
  Left _    -> yield $ TextMessage $ BS.unpack str
  Right cmd -> yield cmd


unblockC :: Monad m => Conduit BS.ByteString m BS.ByteString
unblockC = awaitForever $ \block -> case ord $ BS.head block of
  21 -> if readId block `elem` [
          37 -- BLK_FINGER
        , 43 -- BLK_GAMES
        , 51 -- BLK_HISTORY
        , 84 -- BLK_PARTNER
        , 107 -- BLK_SAY
        , 132 -- BLK_TELL
        , 146 -- BLK_WHO
        ]
        then yield block else sourceList (lines' $ crop block)
  _ -> yield block

crop :: BS.ByteString -> BS.ByteString
crop bs = let lastIdx = maybe 0 (+1) (BS.elemIndexEnd (chr 22) bs) in
          BS.drop lastIdx $ BS.init bs

lines' :: BS.ByteString -> [BS.ByteString]
lines' = filter (not . BS.null) . BS.split '\n'

readId :: BS.ByteString -> Int
readId = read . BS.unpack . BS.takeWhile (/= chr 22) . BS.tail . BS.dropWhile (/= chr 22)


blockC :: Monad m => BS.ByteString -> Conduit BS.ByteString m BS.ByteString
blockC block = awaitForever $ \line -> case ord $ BS.head line of
  21 -> blockC line
  23 -> yield (block `BS.append` line) >> blockC BS.empty -- ^ don't ever delete this again!
  _ | BS.null block -> unless (line `elem` ignore) $ yield line
    | otherwise -> blockC $ block `BS.append` line
  where ignore = [BS.pack "\n", BS.pack "\a\n"]


-- remember!! "fics% \NAK4\SYN87\SYNThere are no offers pending to other players."
linesC :: Monad m => Conduit BS.ByteString m BS.ByteString
linesC = awaitForever $ sourceList . filter (not . BS.null) . fmap dropPrompt . BS.split '\r'


dropPrompt :: BS.ByteString -> BS.ByteString
dropPrompt line
  | BS.take promptSz line == prompt = BS.drop promptSz line
  | otherwise = line
  where prompt = BS.pack "fics% "
        promptSz = BS.length prompt


logStreamC :: MonadIO m => Conduit BS.ByteString m BS.ByteString
logStreamC = awaitForever $ \block -> do
  liftIO $ debugM logger $ showLitString (BS.unpack block) ""
  yield block


convertMoveToResult :: Move.Move -> Message
convertMoveToResult move' = GameResult $ R.Result (Move.gameId move') (Move.nameW move') (Move.nameB move') result (turnToGameResult colorTurn)
  where
    colorTurn = Move.turn move'
    result = Move.namePlayer colorTurn move' ++ " checkmated"
    turnToGameResult Black = R.WhiteWins
    turnToGameResult White = R.BlackWins

