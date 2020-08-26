{-# LANGUAGE LambdaCase, MultiWayIf #-}

module Macbeth.Wx.Utils (
  FrameConfig(..),
  FrameActions(..),
  basicFrame,
  eventLoop,
  registerWxCloseEventListener,
  registerWxCloseEventListenerWithThreadId,
  listItemRightClickEvent,
  toWxColor,
  getDisplaySelection,
  getMayDisplaySelection,
  onlyKey,
  keyWithMod,
  getUserOrAppFile,
  setStatus
) where

import Macbeth.Fics.Message
import Macbeth.Fics.Api.Api

import Control.Monad.Cont
import Control.Concurrent
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import System.Directory
import System.FilePath
import Paths


data FrameConfig = FrameConfig {
    fCreate :: [Prop (Frame ())] -> IO (Frame ())
  , fTitle :: String
  , hasStatusField :: Bool
}

data FrameActions = FrameActions {
  closeFrame :: IO (),
  setDefaultButton :: Button () -> IO ()
}

basicFrame :: FrameConfig -> Chan Message -> Cont (IO ()) (Panel (), StatusField, FrameActions)
basicFrame config chan = cont $ \callback -> do
  f <- fCreate config [text := fTitle config]
  p <- panel f []
  status <- statusField []

  _ <- registerWxCloseEventListener f chan

  windowOnKeyDown p (\evt -> if
    | onlyEsc evt -> close f
    | keyWithMod evt 'W' justControl -> close f
    | otherwise -> return ())

  callback (
    p,
    status,
    FrameActions (close f) (\btn -> set f [ defaultButton := btn]))
  when (hasStatusField config) $ set f [ statusBar := [status] ]
  set f [ layout := fill $ widget p ]


eventLoop :: Int -> Chan Message -> MVar Message -> Frame () -> IO ()
eventLoop eventId chan vCmd f = readChan chan >>= putMVar vCmd >>
  commandEventCreate wxEVT_COMMAND_MENU_SELECTED eventId >>= evtHandlerAddPendingEvent f >>
  eventLoop eventId chan vCmd f


registerWxCloseEventListener :: Frame () -> Chan Message -> IO ()
registerWxCloseEventListener f chan = do
  vCmd <- newEmptyMVar
  threadId <- forkIO $ eventLoop (wxID_HIGHEST + 13) chan vCmd f
  windowOnDestroy f $ killThread threadId

  evtHandlerOnMenuCommand f (wxID_HIGHEST + 13) $ takeMVar vCmd >>= \case
    WxClose -> close f
    _ -> return ()


registerWxCloseEventListenerWithThreadId :: Frame () -> Chan Message -> IO ThreadId
registerWxCloseEventListenerWithThreadId f chan = do
  vCmd <- newEmptyMVar
  threadId <- forkIO $ eventLoop (wxID_HIGHEST + 13) chan vCmd f

  evtHandlerOnMenuCommand f (wxID_HIGHEST + 13) $ takeMVar vCmd >>= \case
    WxClose -> close f
    _ -> return ()

  return threadId


listItemRightClickEvent :: ListCtrl a -> (Graphics.UI.WXCore.ListEvent () -> IO ()) -> IO ()
listItemRightClickEvent lc eventHandler
  = windowOnEvent lc [wxEVT_COMMAND_LIST_ITEM_RIGHT_CLICK] eventHandler listHandler
    where
      listHandler :: Graphics.UI.WXCore.Event () -> IO ()
      listHandler evt = eventHandler $ objectCast evt


onlyKey :: EventKey -> Char -> Bool
onlyKey evt c = (keyKey evt == KeyChar c) && isNoneDown (keyModifiers evt)


onlyEsc :: EventKey -> Bool
onlyEsc evt = (keyKey evt == KeyEscape) && isNoneDown (keyModifiers evt)


keyWithMod :: EventKey -> Char -> Modifiers -> Bool
keyWithMod evt c modifier = (keyKey evt == KeyChar c) && (keyModifiers evt == modifier)


getDisplaySelection :: Choice () -> IO String
getDisplaySelection c = get c selection >>= get c . item

getMayDisplaySelection :: Choice () -> IO (Maybe String)
getMayDisplaySelection c = do
  selectionIdx <- get c selection
  case selectionIdx of
    -1 -> return Nothing
    _ -> Just <$> get c (item selectionIdx)

toWxColor :: PColor -> Color
toWxColor White = Graphics.UI.WXCore.white
toWxColor Black = Graphics.UI.WXCore.black


setStatus :: StatusField -> String -> IO ()
setStatus status msg = set status [ text := msg ]

getUserOrAppFile :: FilePath -> FilePath -> IO FilePath
getUserOrAppFile userDir' file' = do
  let fullPath' = userDir' </> file'
  exists' <- doesFileExist fullPath'
  if exists'
    then return fullPath'
    else getDataFileName file'
