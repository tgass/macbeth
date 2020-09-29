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
import           Macbeth.Fics.Api.Api
import           Macbeth.Fics.Api.Player
import           Macbeth.Wx.Chat
import           Macbeth.Wx.Config.Sounds
import           Macbeth.Wx.Config.UserConfig (chatOrDef)
import           Macbeth.Wx.RuntimeEnv 

wxChatRegistry :: RuntimeEnv -> Chan Message -> IO (Message -> IO ())
wxChatRegistry env chan = return $ \case

  Says (UserHandle username _) Nothing msg -> openIfUntracked (UserChat username) $ Receiving username msg
  
  Tells (UserHandle username _) Nothing msg -> openIfUntracked (UserChat username) $ Receiving username msg
  
  Tells (UserHandle username _) (Just channelId) msg -> openIfUntracked (ChannelChat channelId) $ Receiving username msg
  
  Says (UserHandle username _) (Just gameId) msg -> openIfUntracked (GameChat gameId) $ Receiving username msg
  
  Whispers (UserHandle username _) _ gameId msg -> openIfUntracked (GameChat gameId) $ Receiving username msg
  
  Kibitzes (UserHandle username _) _ gameId msg -> openIfUntracked (GameChat gameId) $ Receiving username msg
  
  _ -> return ()

  where
    openIfUntracked :: ChatId -> Receiving -> IO ()
    openIfUntracked chatId msg = do
      isTracked <- isTrackedChat env chatId
      unless isTracked $ do
        env `trackChat` chatId
        dupChan chan >>= wxChat env chatId (Just msg)
