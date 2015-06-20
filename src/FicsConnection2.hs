{-# LANGUAGE OverloadedStrings #-}

module FicsConnection2 (
  ficsConnection
) where

import Api
import CommandMsg
import CommandMsgParser
import Game
import Move
import Seek

import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Network (connectTo, PortID (..))

import System.Directory
import System.FilePath
import System.IO

data HelperState = HelperState { gameId' :: Maybe Int }


ficsConnection :: (Handle -> CommandMsg -> IO ()) -> IO Handle
ficsConnection handler = runResourceT $ do
  (releaseSock, hsock) <- allocate (connectTo "freechess.org" $ PortNumber 5000) hClose
  liftIO $ hSetBuffering hsock LineBuffering
  resourceForkIO $ liftIO $ chain handler hsock
  return hsock


chain handler hsock = flip evalStateT (HelperState Nothing) $ transPipe lift
  (CB.sourceHandle hsock) $$
  toCharC =$
  blockC (False, BS.empty) =$
  parseC =$
  stateC =$
  sink (handler hsock)


sink :: (CommandMsg -> IO ()) -> Sink CommandMsg (StateT HelperState IO) ()
sink handler = awaitForever $ liftIO . handler


stateC :: Conduit CommandMsg (StateT HelperState IO) CommandMsg
stateC = awaitForever $ \cmd -> case cmd of
                                  ConfirmMove move -> CL.sourceList $ [GameMove move] ++ (if (isCheckmate move) then [toGameResult move] else [])

                                  g@(NewGame id) -> lift $ modify (\state -> state { gameId' = Just id})

                                  move''@(GameMove move') -> if (isPlayersNewGame move') then do
                                                               state <- lift get
                                                               case gameId' state of
                                                                 (Just id) -> do
                                                                   lift $ put (state {gameId' = Nothing})
                                                                   yield (StartGame id move')
                                                                 otherwise -> yield move''
                                                             else yield move''

                                  otherwise -> yield cmd


parseC :: Conduit BS.ByteString (StateT HelperState IO) CommandMsg
parseC = awaitForever $ \str -> case parseCommandMsg str of
                                  Left _    -> yield $ TextMessage $ BS.unpack str
                                  Right cmd -> case cmd of
                                    -- ! muss vor dem nächsten Schritt ausgepackt werden
                                    (Boxed cmdx) -> CL.sourceList cmdx
                                    otherwise -> yield cmd


blockC :: (Bool, BS.ByteString) -> Conduit Char (StateT HelperState IO) BS.ByteString
blockC (block, p) = awaitForever $ \c -> do
                                    liftIO $ logger c
                                    case p of
                                      "login:" -> yield "login: " >> blockC (False, BS.empty)
                                      "password:" -> yield "password: " >> blockC (False, BS.empty)
                                      _ -> case ord c of
                                            21 -> blockC (True, BS.singleton c)
                                            23 -> yield (p `BS.append` BS.singleton c) >> blockC (False, BS.empty)
                                            10 -> if block then blockC (block, p `BS.append` BS.singleton c)
                                                           else yield p >> blockC (block, BS.empty)
                                            13 -> blockC (block, p) -- ignores \r
                                            _  -> blockC (block, p `BS.append` BS.singleton c)


toCharC :: (Monad m) => Conduit BS.ByteString m Char
toCharC = awaitForever $ CL.sourceList . BS.unpack


toGameResult :: Move -> CommandMsg
toGameResult move = GameResult (Move.gameId move) (namePlayer colorTurn move ++ " checkmated") (turnToGameResult colorTurn)
  where
    colorTurn = turn move
    turnToGameResult Black = WhiteWins
    turnToGameResult White = BlackWins


logger :: Char -> IO ()
logger c = do
  rootDir <- getUserDocumentsDirectory
  createDirectoryIfMissing False $ rootDir </> "XChess"
  appendFile (rootDir </> "XChess" </> "xchess.log") [c]

