{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Macbeth.Fics.FicsConnection (
  ficsConnection
) where

import Macbeth.Fics.AppConfig
import Macbeth.Fics.FicsMessage hiding (gameId)
import Macbeth.Fics.Api.Move hiding (Observing)
import Macbeth.Fics.Parsers.FicsMessageParser

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Char
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List hiding (filter)
import Data.Maybe
import System.Log.Logger
import System.Log.Handler.Simple hiding (priority)
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import Network
import Prelude hiding (log)
import System.IO

import qualified Data.ByteString.Char8 as BS

data HelperState = HelperState { takebackAccptedBy :: Maybe String -- ^ oponent accepted takeback request
                               , observingGameId :: Maybe Int
                               , newGameId :: Maybe Int }


ficsConnection :: IO (Handle, Chan FicsMessage)
ficsConnection = runResourceT $ do
  (_, hsock) <- allocate (connectTo "freechess.org" $ PortNumber 5000) hClose
  liftIO $ hSetBuffering hsock LineBuffering
  liftIO $ initLogger . priority =<< loadAppConfig
  chan <- liftIO newChan
  resourceForkIO $ liftIO $ chain hsock chan
  return (hsock, chan)


chain :: Handle -> Chan FicsMessage -> IO ()
chain h chan = flip evalStateT emptyState $ transPipe lift
  (sourceHandle h) $$
  linesC =$
  blockC BS.empty =$
  unblockC =$
  logStreamC =$
  parseC =$
  stateC =$
  copyC chan =$
  logFicsMessageC =$
  sink chan
  where emptyState = HelperState Nothing Nothing Nothing


sink :: Chan FicsMessage -> Sink FicsMessage (StateT HelperState IO) ()
sink chan = awaitForever $ liftIO . writeChan chan


logFicsMessageC :: Conduit FicsMessage (StateT HelperState IO) FicsMessage
logFicsMessageC = awaitForever $ \cmd -> do
  liftIO $ debugM _fileL (logMsg cmd)
  liftIO $ debugM _consoleL (logMsg cmd)
  yield cmd


copyC :: Chan FicsMessage -> Conduit FicsMessage (StateT HelperState IO) FicsMessage
copyC chan = awaitForever $ \case
  m@(MatchAccepted move) -> do
    chan' <- liftIO $ dupChan chan
    sourceList [m, WxMatchAccepted move chan']
  m@(Observe move) -> do
    chan' <- liftIO $ dupChan chan
    sourceList [m, WxObserve move chan']
  cmd -> yield cmd


stateC :: Conduit FicsMessage (StateT HelperState IO) FicsMessage
stateC = awaitForever $ \cmd -> do
  state <- get
  case cmd of
    GameCreation id -> do
      put $ state {newGameId = Just id}
      sourceNull

    Observing id -> do
      put $ state {observingGameId = Just id}
      sourceNull

    TakebackAccepted username -> do
      put $ state {takebackAccptedBy = Just username}
      sourceNull

    m@(GameMove _ move)
      | isCheckmate move && isOponentMove move -> sourceList $ cmd : [toGameResult move]
      | isNewGameUser move && fromMaybe False ((== gameId move) <$> newGameId state) -> do
          put $ state {newGameId = Nothing }
          sourceList [MatchAccepted move]
      | fromMaybe False ((== gameId move) <$> observingGameId state) -> do
          put $ state {observingGameId = Nothing }
          sourceList [Observe move]
      | isJust $ takebackAccptedBy state -> do
          put $ state {takebackAccptedBy = Nothing}
          sourceList [m{context = Takeback $ takebackAccptedBy state}]
      | otherwise -> sourceList [m]

    _ -> yield cmd


parseC :: Conduit BS.ByteString (StateT HelperState IO) FicsMessage
parseC = awaitForever $ \str -> case parseFicsMessage str of
  Left _    -> yield $ TextMessage $ BS.unpack str
  Right cmd -> yield cmd


unblockC :: Conduit BS.ByteString (StateT HelperState IO) BS.ByteString
unblockC = awaitForever $ \block -> case ord $ BS.head block of
  21 -> if readId block `elem` [
       1 -- game move
    , 10 -- Abort
    , 11 -- Accept (Draw, Abort)
    , 33 -- Decline (ie match offer)
    , 34 -- Draw (Mutual)
    , 80 -- Observing
    , 103 -- Resign
    , 155 -- Seek (Users' seek matches a seek already posted)
    , 158 -- Play (User selects a seek)
    , 56 -- BLK_ISET, seekinfo set.
    ]
    then sourceList (lines $ crop block) else yield block
  _ -> yield block
  where
    crop :: BS.ByteString -> BS.ByteString
    crop bs = let lastIdx = maybe 0 (+1) (BS.elemIndexEnd (chr 22) bs) in
              BS.drop lastIdx $ BS.init bs

    lines :: BS.ByteString -> [BS.ByteString]
    lines bs = fmap (`BS.snoc` '\n') $ init $ filter (not . BS.null) $ BS.split '\n' bs

    readId :: BS.ByteString -> Int
    readId = read . BS.unpack . BS.takeWhile (/= chr 22) . BS.tail . BS.dropWhile (/= chr 22)


blockC :: BS.ByteString -> Conduit BS.ByteString (StateT HelperState IO) BS.ByteString
blockC block = awaitForever $ \line -> case ord $ BS.head line of
  21 -> blockC line
  23 -> yield (block `BS.append` line) >> blockC BS.empty -- ^ don't ever delete this again!
  _ | BS.null block -> unless (line `elem` ignore) $ yield line
    | otherwise -> blockC $ block `BS.append` line
  where ignore = [BS.pack "\n", BS.pack "\a\n"]


-- remember!! "fics% \NAK4\SYN87\SYNThere are no offers pending to other players."
linesC :: Conduit BS.ByteString (StateT HelperState IO) BS.ByteString
linesC = awaitForever $ sourceList . filter (not . BS.null) . fmap dropPrompt . BS.split '\r'


dropPrompt :: BS.ByteString -> BS.ByteString
dropPrompt line
  | BS.take promptSz line == prompt = BS.drop promptSz line
  | otherwise = line
  where prompt = BS.pack "fics% "
        promptSz = BS.length prompt


logStreamC :: Conduit BS.ByteString (StateT HelperState IO) BS.ByteString
logStreamC = awaitForever $ \chunk -> do
  liftIO $ debugM _fileL $ (foldr (.) (showString "") $ fmap showLitChar (BS.unpack chunk)) ""
  yield chunk


toGameResult :: Move -> FicsMessage
toGameResult move = GameResult id reason result
  where (id, reason, result) = toGameResultTuple move


logMsg :: FicsMessage -> String
logMsg NewSeek {} = ""
logMsg RemoveSeeks {} = ""
logMsg cmd = show cmd

_consoleL = "_CONSOLE"
_fileL = "_FILE"

initLogger :: Priority -> IO ()
initLogger prio = do
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName (setLevel prio)
  fileH <- fileHandler "/tmp/macbeth.log" prio >>= \lh -> return $
       setFormatter lh (simpleLogFormatter "$time $msg")
  stdOutH <- streamHandler stdout prio >>= \lh -> return $
       setFormatter lh (simpleLogFormatter "$time $msg")

  updateGlobalLogger _fileL (addHandler fileH)
  updateGlobalLogger _consoleL (addHandler stdOutH)

