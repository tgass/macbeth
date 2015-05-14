{-# LANGUAGE OverloadedStrings #-}

module FicsConnection2 (
  ficsConnection
) where

import Seek2
import CommandMsg
import CommandMsgParser

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
import System.IO


ficsConnection :: (Handle -> CommandMsg -> IO ()) -> IO Handle
ficsConnection handler = runResourceT $ do
                          (releaseSock, hsock) <- allocate (connectTo "freechess.org" $ PortNumber 5000) hClose
                          liftIO $ hSetBuffering hsock LineBuffering
                          resourceForkIO $ liftIO $ chain handler hsock
                          return hsock


chain handler hsock = flip evalStateT [] $ transPipe lift
                                            (CB.sourceHandle hsock) $$
                                            toCharC =$
                                            blockC (False, BS.empty) =$
                                            parseC =$
                                            stateC =$ sink (handler hsock)


sink :: (CommandMsg -> IO ()) -> Sink CommandMsg (StateT [Seek2] IO) ()
sink handler = awaitForever $ liftIO . handler


stateC :: Conduit CommandMsg (StateT [Seek2] IO) CommandMsg
stateC = awaitForever $ \cmd -> case cmd of
                                 ClearSeek -> (lift $ put []) >> (yield $ Sought {seekList = []}) >> stateC
                                 NewSeek s -> do
                                      seeks <- lift $ get
                                      let seeks' = seeks ++ [s]
                                      lift $ put seeks'
                                      yield $ Sought {seekList = seeks'}
                                      stateC
                                 RemoveSeeks ids' -> do
                                      seeks <- lift $ get
                                      let seeks' = filter (\s -> not $ Seek2.id s `elem` ids') seeks
                                      lift $ put seeks'
                                      yield $ Sought {seekList = seeks'}
                                      stateC
                                 _ -> yield cmd >> stateC


parseC :: (Monad m) => Conduit BS.ByteString m CommandMsg
parseC = awaitForever $ \str -> case parseCommandMsg str of
                                  Left _    -> yield (TextMessage str) >> parseC
                                  Right msg -> case msg of
                                                SeekInfoBlock bs -> CL.sourceList bs >> parseC
                                                SeekMatchesAlreadyPosted c1 c2 -> CL.sourceList [c1, c2] >> parseC
                                                _ -> yield msg >> parseC


blockC :: (Monad m) =>  (Bool, BS.ByteString) -> Conduit Char m BS.ByteString
blockC (block, p) = awaitForever $ \c -> case p of
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


