{-# LANGUAGE TemplateHaskell #-}

module Macbeth.Wx.Config.UserConfig where

import           Control.Lens
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics
import           Macbeth.Utils.Utils
import           Macbeth.Fics.Commands.Seek (SeekConfig)
import           Macbeth.Wx.Config.Sounds
import           Macbeth.Wx.Config.BoardConfig
import           Macbeth.Wx.Config.SeekConfig (SeekConfigFormat)
import qualified Macbeth.Wx.Config.SeekConfig as SeekConfig
import           Paths
import           System.Directory
import           System.FilePath
import           System.Log.Logger

logger :: String
logger = "Macbeth.Wx.Config.UserConfig"

data Config = Config {
    directory :: FilePath
  , _cAutologin :: Bool
  , fontSize :: Int
  , _cUser :: Maybe User
  , boardConfig :: Maybe BoardConfigFormat
  , sounds :: Maybe Sounds
  , seekConfig :: Maybe SeekConfigFormat
} deriving (Show, Generic)

instance ToJSON Config where
  toJSON = genericToJSON (customOptions "_c")

instance FromJSON Config where
  parseJSON = genericParseJSON (customOptions "_c")

data User = User {
    _uUsername :: String
  , _uPassword :: String
  } deriving (Show, Generic)

instance ToJSON User where
  toJSON = genericToJSON (customOptions "_u")

instance FromJSON User where
  parseJSON = genericParseJSON (customOptions "_u")

makeLenses ''Config
makeLenses ''User


chatOrDef :: Sounds -> ChatS
chatOrDef = fromMaybe chatS . chat


soundsOrDef :: Config -> Sounds
soundsOrDef = fromMaybe defaultSounds . sounds


initConfig :: IO Config
initConfig = do
  createDirectoryIfMissing False =<< getMacbethUserDataDir ""
  configExists <- doesFileExist =<< getMacbethUserDataDir "macbeth.yaml"
  unless configExists $ do
    dir <- (</> "Macbeth") <$> getUserDocumentsDirectory
    createDirectoryIfMissing False dir
    saveConfig $ defaultConfig dir
  loadConfig


loadConfig :: IO Config
loadConfig = do
  file <- getMacbethUserDataDir "macbeth.yaml" 
  infoM logger $ "Loading user config from " ++ file
  decodeFileEither file >>= \case
    Left err -> do
      errorM logger $ prettyPrintParseException err
      error "parse error"
    Right config -> return $ setDefaults config
  

-- make sure that default values are always set
-- this is important specially when the configuration is shown to the user
setDefaults :: Config -> Config
setDefaults c = c{sounds = Just (soundsOrDef'{chat = Just chatOrDef'}), boardConfig = boardConfig'', seekConfig = seekConfig' }
  where
    soundsOrDef' = soundsOrDef c
    chatOrDef' = chatOrDef soundsOrDef'
    -- Building a full board config: 
    -- first: set boardConfig in case it is missing
    --TODO: Use some lenses here?
    boardConfig' = boardConfig c <|> Just defaultBoardConfig
    -- second: set tiles in case only they are missing
    boardConfig'' = (\b -> b{ whiteTile = whiteTile b <|> Just defaultWhiteTile
                            , blackTile = blackTile b <|> Just defaultBlackTile 
                            , pieceSet = pieceSet b <|> Just defaultPieceSet
                            , boardSize = (max minBoardSize <$> boardSize b) <|> Just defaultBoardSize
                            }) <$> boardConfig'

    seekConfig' = fmap SeekConfig.setDefault (seekConfig c) <|> Just SeekConfig.defaultFormat

saveConfig :: Config -> IO ()
saveConfig config = getMacbethUserDataDir "macbeth.yaml" >>= flip encodeFile config


saveCredentials :: String -> String -> IO ()
saveCredentials username password = do
  config <- loadConfig
  saveConfig $ config {_cUser = Just $ User username (encrypt password), _cAutologin = True}

setDefaultHighlightSolid :: Config -> Config
setDefaultHighlightSolid config = 
  let bc = boardConfig config >>= \c -> return c{highlightConfig = Just defaultHighlightConfig}
  in config{boardConfig = bc}


setDefaultHighlightHatched :: Config -> Config
setDefaultHighlightHatched config = 
  let bc = boardConfig config >>= \c -> return c{highlightConfig = Just defaultHighlightHatched}
  in config{boardConfig = bc}


removeUser :: Config -> Config
removeUser c = c & cUser .~ Nothing


defaultConfig :: String -> Config
defaultConfig dir = Config {
    fontSize = 12
  , directory = dir
  , _cAutologin = False
  , boardConfig = Just defaultBoardConfig
  , _cUser = Nothing
  , sounds = Just defaultSounds
  , seekConfig = Just SeekConfig.defaultFormat
}

getSeekConfig :: Config -> SeekConfig
getSeekConfig config = SeekConfig.convert $ fromMaybe SeekConfig.defaultFormat $ seekConfig config


minBoardSize :: Int
minBoardSize = 220
