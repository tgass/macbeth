{-# LANGUAGE TemplateHaskell #-}

module Macbeth.Wx.CommandHistory where

import           Control.Lens
import           Safe


data CommandHistory = CommandHistory {
    _chIdx :: Int
  , _chCommands :: [String]
  } deriving (Show, Eq)


makeLenses ''CommandHistory


maxSize :: Int
maxSize = 30


load :: CommandHistory -> IO ()
load = undefined


save :: CommandHistory -> IO ()
save = undefined


empty :: CommandHistory
empty = CommandHistory 0 []


push :: String -> CommandHistory -> CommandHistory
push cmd history = history 
  & chIdx .~ 0
  & chCommands %~ cons cmd


up :: CommandHistory -> (Maybe String, CommandHistory)
up history@(CommandHistory idx commands) = 
  case commands `atMay` succ idx of
    Just _ -> (commands `atMay` idx, history & chIdx %~ succ)
    Nothing -> (commands `atMay` idx, history)


down :: CommandHistory -> (Maybe String, CommandHistory)
down history@(CommandHistory 0 _) = (Nothing, history)
down history@(CommandHistory idx commands) = (Just $ commands !! pred idx, history & chIdx %~ pred)


