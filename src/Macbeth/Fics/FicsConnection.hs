{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Macbeth.Fics.FicsConnection (
  ficsConnection
) where

import Macbeth.Fics.AppConfig
import Macbeth.Fics.FicsMessage hiding (gameId)
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move hiding (Observing)
import Macbeth.Fics.Parsers.FicsMessageParser
import qualified Macbeth.Fics.Api.Result as R

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
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

data HelperState = HelperState { takebackAccptedBy :: Maybe (Maybe String)
                               , illegalMove :: Maybe String }


ficsConnection :: IO (Handle, Chan FicsMessage)
ficsConnection = runResourceT $ do
  (_, hsock) <- allocate (connectTo "freechess.org" $ PortNumber 5000) hClose
  liftIO $ hSetBuffering hsock LineBuffering
  liftIO $ initLogger . priority =<< loadAppConfig
  chan <- liftIO newChan
  _ <- resourceForkIO $ liftIO $ chain hsock chan
  return (hsock, chan)


chain :: Handle -> Chan FicsMessage -> IO ()
chain h chan = flip evalStateT emptyState $ transPipe lift
  (sourceHandle h) $$
  linesC =$
  blockC BS.empty =$
  logStreamC =$
  unblockC =$
  parseC =$
  stateC =$
  copyC chan =$
  logFicsMessageC =$
  sink chan
  where emptyState = HelperState Nothing Nothing


sink :: Chan FicsMessage -> Sink FicsMessage (StateT HelperState IO) ()
sink chan = awaitForever $ liftIO . writeChan chan


logFicsMessageC :: Conduit FicsMessage (StateT HelperState IO) FicsMessage
logFicsMessageC = awaitForever $ \cmd -> do
  _ <- liftIO $ runMaybeT ((MaybeT $ return $ logMsg cmd) >>= \msg -> do
    lift $ debugM fileLogger msg
    lift $ debugM consoleLogger msg)
  yield cmd


copyC :: Chan FicsMessage -> Conduit FicsMessage (StateT HelperState IO) FicsMessage
copyC chan = awaitForever $ \case
  m@(GameCreation gameProperties) -> do
    chan' <- liftIO $ dupChan chan
    sourceList [m, WxNewGame gameProperties chan']
  m@(Observing gameProperties) -> do
    chan' <- liftIO $ dupChan chan
    sourceList [m, WxNewGame gameProperties chan']
  cmd -> yield cmd


stateC :: Conduit FicsMessage (StateT HelperState IO) FicsMessage
stateC = awaitForever $ \cmd -> do
  state' <- get
  case cmd of

    TakebackAccepted username -> put (state' {takebackAccptedBy = Just username}) >> sourceNull

    IllegalMove m -> put (state' {illegalMove = Just m}) >> sourceNull

    m@(GameMove _ move')
      | isCheckmate move' && isOponentMove move' -> sourceList $ cmd : [moveToResult move']
      | isJust $ takebackAccptedBy state' -> do
          put $ state' {takebackAccptedBy = Nothing}
          sourceList [m{context = Takeback $ fromJust $ takebackAccptedBy state'}]
      | isJust $ illegalMove state' -> do
          put $ state' {illegalMove = Nothing}
          sourceList [m{context = Illegal $ fromJust $ illegalMove state'}]
      | otherwise -> sourceList [m]

    _ -> yield cmd


parseC :: Conduit BS.ByteString (StateT HelperState IO) FicsMessage
parseC = awaitForever $ \str -> case parseFicsMessage str of
  Left _    -> yield $ TextMessage $ BS.unpack str
  Right cmd -> yield cmd


unblockC :: Conduit BS.ByteString (StateT HelperState IO) BS.ByteString
unblockC = awaitForever $ \block -> case ord $ BS.head block of
  21 -> if readId block `elem` [ 107, 132, 43, 146, 84, 37, 51, 158]
        then yield block else sourceList (lines' $ crop block)
  _ -> yield block
  where
    crop :: BS.ByteString -> BS.ByteString
    crop bs = let lastIdx = maybe 0 (+1) (BS.elemIndexEnd (chr 22) bs) in
              BS.drop lastIdx $ BS.init bs

    lines' :: BS.ByteString -> [BS.ByteString]
    lines' bs = fmap (`BS.snoc` '\n') $ init $ filter (not . BS.null) $ BS.split '\n' bs

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
  liftIO $ debugM fileLogger $ (foldr (.) (showString "") $ fmap showLitChar (BS.unpack chunk)) ""
  yield chunk


moveToResult :: Move -> FicsMessage
moveToResult move' = GameResult $ R.Result (gameId move') (nameW move') (nameB move') result (turnToGameResult colorTurn)
  where
    colorTurn = turn move'
    result = namePlayer colorTurn move' ++ " checkmated"
    turnToGameResult Black = R.WhiteWins
    turnToGameResult White = R.BlackWins


logMsg :: FicsMessage -> Maybe String
logMsg NewSeek {} = Nothing
logMsg RemoveSeeks {} = Nothing
logMsg cmd = Just $ show cmd


consoleLogger :: String
consoleLogger = "_CONSOLE"


fileLogger :: String
fileLogger = "_FILE"


initLogger :: Priority -> IO ()
initLogger prio = do
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName (setLevel prio)
  fileH <- fileHandler "/tmp/macbeth2.log" prio >>= \lh -> return $
       setFormatter lh (simpleLogFormatter "$time $msg")
  stdOutH <- streamHandler stdout prio >>= \lh -> return $
       setFormatter lh (simpleLogFormatter "$time $msg")

  updateGlobalLogger fileLogger (addHandler fileH)
  updateGlobalLogger consoleLogger (addHandler stdOutH)

