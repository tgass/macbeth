{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Macbeth.Fics.FicsConnection (
  ficsConnection
) where

import Macbeth.Fics.Configuration
import Macbeth.Fics.FicsMessage hiding (gameId)
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Parsers.FicsMessageParser

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Char
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import Data.Maybe
import Data.Time
import Network
import System.FilePath
import System.IO

import qualified Data.ByteString.Char8 as BS


data HelperState = HelperState { config :: Config
                               , takebackAccptedBy :: Maybe String -- ^ oponent accepted takeback request
                               , newGameId :: Maybe Int }


ficsConnection :: IO (Handle, Chan FicsMessage)
ficsConnection = runResourceT $ do
  (_, hsock) <- allocate (connectTo "freechess.org" $ PortNumber 5000) hClose
  liftIO $ hSetBuffering hsock LineBuffering
  config <- liftIO loadConfig
  chan <- liftIO newChan
  resourceForkIO $ liftIO $ chain hsock config chan
  return (hsock, chan)


chain :: Handle -> Config-> Chan FicsMessage -> IO ()
chain h config chan = flip evalStateT (HelperState config Nothing Nothing) $ transPipe lift
  (sourceHandle h) $$
  linesC =$
  blockC BS.empty =$
  fileLoggerC =$
  parseC =$
  stateC =$
  copyC chan =$
  loggingC =$
  sink chan


sink :: Chan FicsMessage -> Sink FicsMessage (StateT HelperState IO) ()
sink chan = awaitForever $ liftIO . writeChan chan


loggingC :: Conduit FicsMessage (StateT HelperState IO) FicsMessage
loggingC = awaitForever $ \cmd -> do
  logToStdOut <- (stdOut . logging . config) `fmap` get
  when logToStdOut $ liftIO (printCmdMsg cmd)
  yield cmd


copyC :: Chan FicsMessage -> Conduit FicsMessage (StateT HelperState IO) FicsMessage
copyC chan = awaitForever $ \case
  m@(MatchAccepted match) -> do
    chan' <- liftIO $ dupChan chan
    sourceList [m, WxMatchAccepted match chan']
  cmd -> yield cmd


stateC :: Conduit FicsMessage (StateT HelperState IO) FicsMessage
stateC = awaitForever $ \cmd -> do
  state <- get
  case cmd of
    Boxed cmds -> sourceList cmds

    GameCreation id -> do
      put $ state {newGameId = Just id}
      sourceNull

    TakebackAccepted username -> do
      put $ state {takebackAccptedBy = Just username}
      sourceNull

    m@(GameMove _ move)
      | isCheckmate move && isOponentMove move -> sourceList $ cmd : [toGameResult move]
      | isNewGameUser move && fromMaybe False ((== gameId move) <$> newGameId state) -> do
          put $ state {newGameId = Nothing }
          sourceList [MatchAccepted move]
      | isJust $ takebackAccptedBy state -> do
          put $ state {takebackAccptedBy = Nothing}
          sourceList [m{context = Takeback $ takebackAccptedBy state}]
      | otherwise -> sourceList [m]

    _ -> yield cmd


parseC :: Conduit BS.ByteString (StateT HelperState IO) FicsMessage
parseC = awaitForever $ \str -> case parseFicsMessage str of
  Left _    -> yield $ TextMessage $ BS.unpack str
  Right cmd -> yield cmd


blockC :: BS.ByteString -> Conduit BS.ByteString (StateT HelperState IO) BS.ByteString
blockC block = awaitForever $ \line -> case ord $ BS.head line of
  21 -> blockC line
  23 -> yield (block `BS.append` line) >> blockC BS.empty -- ^ don't ever delete this again!
  _ | BS.null block -> when (line /= newline) (yield line) >> blockC BS.empty
    | otherwise -> blockC (block `BS.append` line)
  where newline = BS.pack "\n"


-- remember!! "fics% \NAK4\SYN87\SYNThere are no offers pending to other players."
linesC :: Conduit BS.ByteString (StateT HelperState IO) BS.ByteString
linesC = awaitForever $ sourceList . Prelude.filter (not . BS.null) . fmap dropPrompt . BS.split '\r'


dropPrompt :: BS.ByteString -> BS.ByteString
dropPrompt line
  | BS.take promptSz line == prompt = BS.drop promptSz line
  | otherwise = line
  where prompt = BS.pack "fics% "
        promptSz = BS.length prompt


fileLoggerC :: Conduit BS.ByteString (StateT HelperState IO) BS.ByteString
fileLoggerC = awaitForever $ \chunk -> do
   config <- config `fmap` get
   when (file (logging config) && isJust (directory config)) $
     liftIO $ logFile (fromJust $ directory config) chunk
   yield chunk


toGameResult :: Move -> FicsMessage
toGameResult move = GameResult id reason result
  where (id, reason, result) = toGameResultTuple move


printCmdMsg :: FicsMessage -> IO ()
printCmdMsg NewSeek {} = return ()
printCmdMsg RemoveSeeks {} = return ()
printCmdMsg cmd = print cmd


logFile :: FilePath -> BS.ByteString -> IO ()
logFile path chunk = do
  date <- fmap (formatTime Data.Time.defaultTimeLocale "%Y-%m-%d_") getZonedTime
  dateTime <- fmap (formatTime Data.Time.defaultTimeLocale "%Y-%m-%d %H-%M-%S: ") getZonedTime
  appendFile (path </> date ++ "macbeth.log") $
    (foldr (.) (showString $ "\n" ++ dateTime) $ fmap showLitChar (BS.unpack chunk)) ""

