module Macbeth.Wx.Configuration (
  wxConfiguration
) where

import           Control.Lens hiding (set)
import           Control.Monad.Cont
import           Control.Concurrent.Chan
import           Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe (fromMaybe)
import qualified Data.Yaml as Y
import           Graphics.UI.WX hiding (fontSize)
import           Graphics.UI.WXCore
import           Macbeth.Fics.Message
import           Macbeth.Wx.Config.Sounds
import           Macbeth.Wx.RuntimeEnv
import           Macbeth.Wx.Utils
import qualified Macbeth.Wx.Config.UserConfig as C
import           Macbeth.Wx.Config.UserConfig (cUser)
import qualified Macbeth.Wx.Config.BoardConfig as BC


wxConfiguration :: RuntimeEnv -> Chan Message -> IO ()
wxConfiguration env chan = runCont (basicFrame (frameConfig env) chan) (setupFrame env)


frameConfig :: RuntimeEnv -> FrameConfig
frameConfig _ = FrameConfig {
  fCreate = frame,
  fTitle = "Macbeth (version: " ++ getVersion ++ ")",
  hasStatusField = True
}


setupFrame :: RuntimeEnv -> (Panel (), StatusField, FrameActions) -> IO ()
setupFrame env (p, status, _) = do
  ct <- textCtrlEx p (wxTE_MULTILINE .+. wxTE_RICH2 .+. wxTE_AUTO_URL) [font := fontFixed]
  showConfig ct

  b_current <- button p [text := "Reset", on command := showConfig ct]
  b_resetSounds <- button p [text := "Reset Sounds", on command := reset ct status (\c -> c{C.sounds = Just defaultSounds})]
  b_resetHighlights <- button p [text := "Reset Highlights (solid)", on command := reset ct status C.setDefaultHighlightSolid]
  b_resetHighlightsHatched <- button p [text := "Reset Highlights (hatched)", on command := reset ct status C.setDefaultHighlightHatched]
  b_save <- button p [ text := "Save", on command := save env ct status]

  set p [ layout :=  margin 10 $ column 10 [
                       boxed "Configuration" $ fill $ minsize (Size 530 480) $ widget ct
                     , hfloatRight $ row 5 [widget b_current, widget b_resetHighlights, widget b_resetHighlightsHatched, widget b_resetSounds, widget b_save]]
        ]


showConfig :: TextCtrl() -> IO ()
showConfig ct = do
  config <- C.loadConfig
  set ct [text := comments ++ BS.unpack (Y.encode $ C.removeUser config)]


reset :: TextCtrl() -> StatusField -> (C.Config -> C.Config) -> IO ()
reset ct status f = do
  mConfig <- Y.decode . BS.pack <$> get ct text
  case mConfig of 
    Just config -> set ct [text := comments ++ BS.unpack (Y.encode $ C.removeUser $ f config)]
    Nothing -> setStatus status "Could not reset. Illegal format."


save :: RuntimeEnv -> TextCtrl () -> StatusField -> IO ()
save env ct status = either (const $ setStatus status "Illegal file format.") return =<< runExceptT (parseAndSave env ct status)


parseAndSave :: RuntimeEnv -> TextCtrl () -> StatusField -> ExceptT Y.ParseException IO ()
parseAndSave env ct status = do
  oldConf <- liftIO C.loadConfig
  newConf <- ExceptT $ (Y.decodeEither' . BS.pack) <$> get ct text
  liftIO $ do
    C.saveConfig $ newConf & cUser .~ (oldConf ^. cUser)
    setStatus status "Configuration saved."
    boardConfig <- BC.convert (fromMaybe BC.defaultBoardConfig $ C.boardConfig newConf) (C.directory newConf)
    setBoardConfig env boardConfig
    setSeekConfig env $ C.getSeekConfig newConf


comments :: String
comments = "# Unfortunately, configuration of Macbeth is not beautiful. I hope to change\n\
           \# this in the future! Please read this carefully!\n\
           \# \n\
           \# - Most changes will take effect only after restart (for now).\n\
           \# - If you change the directory make sure it exists, it will not be created for you.\n\
           \# - You can use your own sounds by putting them in MACBETH_USER_DIR/sounds\n\
           \# - Changes in the boardConfig section take effect without restart.\n\
           \# - Board tiles are configured by providing either: \n\
           \#   1) a filename, ie: dave_dark.bmp: \n\
           \#      See https://github.com/tgass/macbeth/tree/master/resources/tiles for your options\n\ 
           \#      You can use your own background tiles by putting them in MACBETH_USER_DIR/tiles\n\
           \#   2) a hex color code, ie: hexFF0011 \n\n"


