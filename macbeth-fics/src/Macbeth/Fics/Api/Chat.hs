{-# LANGUAGE TemplateHaskell #-}

module Macbeth.Fics.Api.Chat where

import Control.Lens
import Macbeth.Fics.Api.Api

data ChatState = Open | Closed deriving (Eq, Show)

data ChatId = UserChat String | GameChat GameId | ChannelChat ChannelId deriving (Show, Eq, Ord)

newtype ChannelId = ChannelId Int deriving (Eq, Ord, Show)

data ChatMessage = From String String | Self String deriving (Show)

data Chat = Chat { _state :: ChatState, _messages :: [ChatMessage] }
makeLenses ''Chat

