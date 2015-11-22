module Macbeth.Utils.PGN (
  saveAsPGN
) where

import Macbeth.Api.Api
import Macbeth.Api.Move
import Macbeth.Api.Game (GameResult)
import qualified Macbeth.Utils.FEN as FEN

import Data.Maybe
import Data.Time
import System.Directory
import System.FilePath
import System.Locale


saveAsPGN :: [Move] -> Maybe GameResult ->  IO ()
saveAsPGN [] _ = return ()
saveAsPGN moves mGameResult = do
  rootDir <- getUserDocumentsDirectory
  createDirectoryIfMissing False $ rootDir </> "Macbeth"
  dateTime <- getZonedTime
  path <- filepath dateTime (head moves)
  appendFile path $ toPGN moves mGameResult dateTime

filepath :: ZonedTime -> Move -> IO FilePath
filepath dateTime m = do
  rootDir <- getUserDocumentsDirectory
  return $ rootDir </> "Macbeth" </> formatTime defaultTimeLocale "%Y-%m-%d" dateTime ++ "_" ++
           formatTime defaultTimeLocale "%H-%M-%S" dateTime ++ "_" ++ nameW m ++ "_vs_" ++ nameB m ++ ".pgn"

toPGN :: [Move] -> Maybe GameResult -> ZonedTime -> String
toPGN [] _ _ = ""
toPGN moves@(m:_) mGameResult dateTime =
  tagsSection m mGameResult dateTime ++ "\n\n" ++
  moveSection (if FEN.available $ head moves then tail moves else moves) ++
  " " ++ maybe "" show mGameResult ++ "\n\n"

moveSection :: [Move] -> String
moveSection = unwords . fmap (toString . toSAN)

tagsSection :: Move -> Maybe GameResult -> ZonedTime -> String
tagsSection m mGameResult dateTime =
  "[Event \"?\"]\n\
  \[Site \"?\"]\n\
  \[Date \"" ++ formatTime defaultTimeLocale "%Y.%m.%d" dateTime ++ "\"]\n\
  \[Time \"" ++ formatTime defaultTimeLocale "%T" dateTime ++ "\"]\n\
  \[Round \"?\"]\n\
  \[White \"" ++ nameW m ++ "\"]\n\
  \[Black \"" ++ nameB m ++ "\"]\n\
  \[Result \"" ++ maybe "?" show mGameResult ++ "\"]\n\
  \[BlackElo \"?\"]\n\
  \[WhiteElo \"?\"]\n\
  \[ECO \"?\"]\n\
  \[TimeControl \"?\"]\n\
  \[SetUp \"" ++ (if FEN.available m then "1" else "?") ++ "\"]\n\
  \[FEN \"" ++ (if FEN.available m then FEN.convert m else "?") ++ "\"]\n"

toSAN :: Move -> (Int, PColor, String)
toSAN m = (moveNumber m, turn m, fromJust $ movePretty m)

toString :: (Int, PColor, String) -> String
toString (num, Black, move) = show num ++ "." ++ move
toString (_, White, move) = move
