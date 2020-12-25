module Macbeth.Wx.Utils where

import           Control.Monad.Cont
import           Control.Concurrent
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.UI.WX hiding (when)
import           Graphics.UI.WXCore hiding (when)
import           Macbeth.Fics.Message
import           Macbeth.Fics.Api.Api
import           System.IO.Unsafe


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

eventLoop :: Frame () -> Int -> Chan Message -> (Message -> IO ()) -> IO ThreadId
eventLoop f eventId chan handler = eventLoopWithThreadId f eventId chan $ \(_, message) -> handler message

eventLoopWithThreadId :: Frame () -> Int -> Chan Message -> ((ThreadId, Message) -> IO ()) -> IO ThreadId
eventLoopWithThreadId f eventId chan handler = do
  vCmd <- newEmptyMVar
  threadId <- forkIO $ forever $ do
    msg <- readChan chan 
    putMVar vCmd msg
    evt <- commandEventCreate wxEVT_COMMAND_MENU_SELECTED eventId 
    evtHandlerAddPendingEvent f evt

  evtHandlerOnMenuCommand f eventId $ takeMVar vCmd >>= \cmd -> handler (threadId, cmd)
  return threadId


registerWxCloseEventListener :: Frame () -> Chan Message -> IO ()
registerWxCloseEventListener f chan = do
  threadId <- eventLoop f (wxID_HIGHEST + 13) chan $ \case
    WxClose -> close f
    _ -> return ()
  windowOnDestroy f $ killThread threadId


registerWxCloseEventListenerWithThreadId :: Frame () -> Chan Message -> IO ThreadId
registerWxCloseEventListenerWithThreadId f chan =
  eventLoop f (wxID_HIGHEST + 13) chan $ \case
    WxClose -> close f
    _ -> return ()


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

{-# NOINLINE flag #-}
flag :: Ptr CInt
flag  =  unsafePerformIO flag'
  where flag' = do
             work <- malloc :: IO (Ptr CInt)
             poke work (fromIntegral wxBK_HITTEST_ONPAGE)
             return work

