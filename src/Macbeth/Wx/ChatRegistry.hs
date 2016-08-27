{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.ChatRegistry (
  wxChatRegistry
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Chat
import Macbeth.Fics.Api.Player
import Macbeth.Wx.Chat
import Macbeth.Wx.RuntimeEnv

import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Map.Strict

data ChatState = Open | Closed deriving (Eq, Show)

type Chat = (ChatState, [ChatMsg])

wxChatRegistry :: RuntimeEnv -> Chan FicsMessage -> IO (FicsMessage -> IO ())
wxChatRegistry env chan = do
  chatMap <- newTVarIO $ fromList [("ROBOadmin", (Open, []))]

  return $ \case

       Chat msg -> let updateAndOpenChat username mGameId = do
                         chat <- updateChatMap chatMap username msg
                         unless (isOpen chat) $ do
                           setChatState chatMap username Open
                           dupChan chan >>= wxChat env mGameId (snd chat)

                   in case msg of
                        Say (UserHandle username _) gameId _ ->
                          updateAndOpenChat username (Just gameId)

                        Tell (UserHandle username _) _ ->
                          updateAndOpenChat username Nothing

                        UserMessage username _ ->
                          updateAndOpenChat username Nothing

                        OpenChat username mGameId ->
                          updateAndOpenChat username mGameId

                        CloseChat username ->
                          setChatState chatMap username Closed

                        _ -> return ()
       _ -> return ()



updateChatMap :: TVar (Map Username Chat) -> Username -> ChatMsg -> IO Chat
updateChatMap chatMap  username msg = atomically $ do
  modifyTVar chatMap $ insertWith appendMessage username (Closed, [msg])
  (! username) <$> readTVar chatMap


-- appendMessage newValue oldValue
appendMessage :: Chat -> Chat -> Chat
appendMessage (_, new_mx) (status, old_mx) = (status, old_mx ++ new_mx)


setChatState :: TVar (Map Username Chat) -> Username -> ChatState -> IO ()
setChatState chatMap username =
  atomically . modifyTVar chatMap . flip adjust username . (_1 .~)


isOpen :: Chat -> Bool
isOpen = (Open ==) . fst
