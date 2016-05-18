{-# LANGUAGE LambdaCase #-}

module Macbeth.Wx.ChatRegistry (
  wxChatRegistry
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Chat
import Macbeth.Fics.Api.Player
import Macbeth.Wx.Chat
import Macbeth.Wx.Sounds

import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe
import System.IO
import qualified Data.Map.Strict as Map

data ChatState = Open | Closed deriving (Eq, Show)

type Chat = (ChatState, [ChatMsg])

wxChatRegistry :: Handle -> Sounds -> Chan FicsMessage -> IO (FicsMessage -> IO ())
wxChatRegistry h sounds chan = do
  chatMap <- newTVarIO $ Map.fromList [("ROBOadmin", emptyChat Open)]

  let handler = \case

       Chat msg -> handleChat h sounds chan chatMap msg

       _ -> return ()

  return handler


handleChat :: Handle -> Sounds -> Chan FicsMessage -> TVar (Map.Map Username Chat) -> ChatMsg -> IO ()
handleChat h sounds chan chatMap msg = do

  let updateAndOpenChat username mGameId = do
        chatState <- updateChatMap chatMap username msg
        unless (isOpen chatState) $ do
          setChat chatMap username Open
          dupChan chan >>= wxChat username mGameId h sounds (snd chatState)

  case msg of
    Say (UserHandle username _) gameId _ -> updateAndOpenChat username (Just gameId)

    Tell (UserHandle username _) _ -> updateAndOpenChat username Nothing

    UserMessage username _ -> updateAndOpenChat username Nothing

    OpenChat username mGameId -> updateAndOpenChat username mGameId

    CloseChat username -> setChat chatMap username Closed

    _ -> return ()


updateChatMap :: TVar (Map.Map Username Chat) -> Username -> ChatMsg -> IO Chat
updateChatMap chatMap  username msg = atomically $ do
  modifyTVar chatMap $ Map.insertWith appendMessage username (Closed, [msg])
  (fromJust . Map.lookup username) <$> readTVar chatMap


setChat :: TVar (Map.Map Username Chat) -> Username -> ChatState -> IO ()
setChat chatMap username =
  atomically . modifyTVar chatMap . flip Map.adjust username . chatState


chatState :: ChatState -> Chat -> Chat
chatState st (_, mx) = (st, mx)


-- appendMessage newValue oldValue
appendMessage :: Chat -> Chat -> Chat
appendMessage (_, new_mx) (status, old_mx) = (status, old_mx ++ new_mx)


isOpen :: Chat -> Bool
isOpen = (Open ==) . fst

emptyChat :: ChatState -> Chat
emptyChat st = (st, [])

