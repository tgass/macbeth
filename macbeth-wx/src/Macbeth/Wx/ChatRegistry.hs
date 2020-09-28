module Macbeth.Wx.ChatRegistry (
  wxChatRegistry
) where

import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Chat
import           Macbeth.Fics.Api.Player
import           Macbeth.Wx.Chat
import           Macbeth.Wx.Config.Sounds
import           Macbeth.Wx.Config.UserConfig (chatOrDef)
import           Macbeth.Wx.RuntimeEnv (RuntimeEnv(), playSound)

wxChatRegistry :: RuntimeEnv -> Chan Message -> IO (Message -> IO ())
wxChatRegistry env chan = do
  chatMap <- newTVarIO $ Map.fromList [(UserChat "ROBOadmin", Chat Open [])]

  return $ \case

    Says (UserHandle username _) Nothing msg -> 
      updateAndOpen chatMap env chan (UserChat username) (From username msg)

    Says (UserHandle username _) (Just gameId) msg -> 
      updateAndOpen chatMap env chan (GameChat gameId) (From username msg)

    Tells (UserHandle username _) Nothing msg -> 
      updateAndOpen chatMap env chan (UserChat username) (From username msg)

    Tells (UserHandle username _) (Just channelId) msg -> 
      updateAndOpen chatMap env chan (ChannelChat channelId) (From username msg)

    Whispers (UserHandle username _) rating gameId msg -> 
      updateAndOpen chatMap env chan (GameChat gameId) (From username msg)

    Kibitzes (UserHandle username _) rating gameId msg -> 
      updateAndOpen chatMap env chan (GameChat gameId) (From username msg)

    UserMessage chatId msg ->
      updateAndOpen chatMap env chan chatId (Self msg)

    OpenChat chatId -> do
      mChat <- Map.lookup chatId <$> readTVarIO chatMap
      case mChat of 
        Just (Chat state msgs) ->
          unless (state == Open) $ do
            setChatState chatMap chatId Open
            dupChan chan >>= wxChat env chatId msgs
        Nothing -> do
          initChat chatMap chatId
          dupChan chan >>= wxChat env chatId []

    CloseChat chatid -> setChatState chatMap chatid Closed

    _ -> return ()


updateAndOpen :: TVar (Map ChatId Chat) -> RuntimeEnv -> Chan Message -> ChatId -> ChatMessage -> IO ()
updateAndOpen chatMap env chan chatId msg = do
  Chat state msgs <- updateChatMap chatMap chatId msg
  unless (state == Open) $ do
    setChatState chatMap chatId Open
    dupChan chan >>= wxChat env chatId msgs


updateChatMap :: TVar (Map ChatId Chat) -> ChatId -> ChatMessage -> IO Chat
updateChatMap chatMapVar chatId msg = atomically $ do
  chatMap <- readTVar chatMapVar
  let newMap = Map.insertWith appendMessage chatId (Chat Closed [msg]) chatMap
  writeTVar chatMapVar newMap
  return $ newMap Map.! chatId


-- appendMessage newValue oldValue
appendMessage :: Chat -> Chat -> Chat
appendMessage (Chat _ new_mx) (Chat status' old_mx) = Chat status' (old_mx ++ new_mx)


setChatState :: TVar (Map ChatId Chat) -> ChatId -> ChatState -> IO ()
setChatState chatMapVar chatId state' = atomically $
  modifyTVar chatMapVar $ Map.adjust (state .~ state') chatId

initChat :: TVar (Map ChatId Chat) -> ChatId -> IO ()
initChat chatMapVar chatId = atomically $ modifyTVar chatMapVar $ Map.insert chatId (Chat Open [])
