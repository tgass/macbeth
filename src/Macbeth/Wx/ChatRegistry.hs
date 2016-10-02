{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Macbeth.Wx.ChatRegistry (
  wxChatRegistry
) where

import qualified Macbeth.Fics.FicsMessage as F
import Macbeth.Fics.Api.Chat
import Macbeth.Fics.Api.Player
import Macbeth.Wx.Chat
import Macbeth.Wx.Config.Sounds
import Macbeth.Wx.Config.UserConfig (chatOrDef)
import Macbeth.Wx.RuntimeEnv (RuntimeEnv(), playSound)

import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Map.Strict

data ChatState = Open | Closed deriving (Eq, Show)

data Chat = Chat { _state :: ChatState, _messages :: [ChatMsg] }
makeLenses ''Chat

wxChatRegistry :: RuntimeEnv -> Chan F.FicsMessage -> IO (F.FicsMessage -> IO ())
wxChatRegistry env chan = do
  chatMap <- newTVarIO $ fromList [("ROBOadmin", Chat Open [])]

  return $ \case

       F.Chat msg -> let updateAndOpenChat username mGameId = do
                         (Chat state' msgs) <- updateChatMap chatMap username msg
                         unless (state' == Open) $ do
                           setChatState chatMap username Open
                           dupChan chan >>= wxChat env username mGameId msgs

                   in case msg of
                        Say (UserHandle username _) gameId _ -> do
                          updateAndOpenChat username (Just gameId)
                          playSound env (say . chatOrDef)

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
  modifyTVar chatMap $ insertWith appendMessage username $ Chat Closed [msg]
  (! username) <$> readTVar chatMap


-- appendMessage newValue oldValue
appendMessage :: Chat -> Chat -> Chat
appendMessage (Chat _ new_mx) (Chat status old_mx) = Chat status (old_mx ++ new_mx)


setChatState :: TVar (Map Username Chat) -> Username -> ChatState -> IO ()
setChatState chatMap username state' = atomically $
  modifyTVar chatMap $ adjust (state .~ state') username

