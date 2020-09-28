{-# LANGUAGE TemplateHaskell #-}

module Macbeth.Fics.Api.Chat where

import Control.Lens
import Macbeth.Fics.Api.Api

data ChatState = Open | Closed deriving (Eq, Show)

data ChatId = UserChat String | GameChat GameId | ObservingChat GameId | ChannelChat ChannelId deriving (Show, Eq, Ord)

newtype ChannelId = ChannelId Int deriving (Eq, Ord, Show)

data ChatMessage = From String String | Self String deriving (Show)

data SpeakMode = TellMode | SayMode | WhisperMode | KibitzMode deriving (Eq, Enum, Bounded)

data Chat = Chat { _state :: ChatState, _messages :: [ChatMessage] }

instance Show SpeakMode where
  show TellMode = "Tell"
  show SayMode = "Say"
  show WhisperMode = "Whisper"
  show KibitzMode = "Kibitz"

makeLenses ''Chat

