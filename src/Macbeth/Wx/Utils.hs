module Macbeth.Wx.Utils (
  eventLoop,
  registerWxCloseEventListener,
  registerWxCloseEventListenerWithThreadId,
  listItemRightClickEvent,
  toWxColor,
  getDisplaySelection,
  staticTextFormatted
) where

import Macbeth.Fics.FicsMessage
import Macbeth.Fics.Api.Api

import Control.Concurrent
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Column, Row)


eventLoop :: Int -> Chan FicsMessage -> MVar FicsMessage -> Frame () -> IO ()
eventLoop id chan vCmd f = readChan chan >>= putMVar vCmd >>
  commandEventCreate wxEVT_COMMAND_MENU_SELECTED id >>= evtHandlerAddPendingEvent f >>
  eventLoop id chan vCmd f

registerWxCloseEventListener :: Frame () -> Chan FicsMessage -> IO ()
registerWxCloseEventListener f chan = do
  vCmd <- newEmptyMVar
  threadId <- forkIO $ eventLoop (wxID_HIGHEST + 13) chan vCmd f
  windowOnDestroy f $ killThread threadId

  evtHandlerOnMenuCommand f (wxID_HIGHEST + 13) $ takeMVar vCmd >>= \cmd -> case cmd of
    WxClose -> close f
    _ -> return ()

registerWxCloseEventListenerWithThreadId :: Frame () -> Chan FicsMessage -> IO ThreadId
registerWxCloseEventListenerWithThreadId f chan = do
  vCmd <- newEmptyMVar
  threadId <- forkIO $ eventLoop (wxID_HIGHEST + 13) chan vCmd f

  evtHandlerOnMenuCommand f (wxID_HIGHEST + 13) $ takeMVar vCmd >>= \cmd -> case cmd of
    WxClose -> close f
    _ -> return ()

  return threadId


listItemRightClickEvent :: ListCtrl a -> (Graphics.UI.WXCore.ListEvent () -> IO ()) -> IO ()
listItemRightClickEvent listCtrl eventHandler
  = windowOnEvent listCtrl [wxEVT_COMMAND_LIST_ITEM_RIGHT_CLICK] eventHandler listHandler
    where
      listHandler :: Graphics.UI.WXCore.Event () -> IO ()
      listHandler evt = eventHandler $ objectCast evt


getDisplaySelection :: Choice () -> IO String
getDisplaySelection c = get c selection >>= get c . item


toWxColor :: PColor -> Color
toWxColor White = Graphics.UI.WXCore.white
toWxColor Black = Graphics.UI.WXCore.black


staticTextFormatted :: Panel () -> String -> IO (StaticText ())
staticTextFormatted p s = staticText p [ text := s
                                       , fontFace := "Avenir Next Medium"
                                       , fontSize := 20
                                       , fontWeight := WeightBold]

