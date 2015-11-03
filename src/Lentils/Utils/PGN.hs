module Lentils.Utils.PGN (
  saveAsPGN
) where

import Lentils.Api.Api
import Lentils.Api.Move
import qualified Lentils.Utils.FEN as FEN

import Data.Maybe
import Data.Time
import System.Directory
import System.FilePath
import System.Locale


saveAsPGN :: [Move] -> IO ()
saveAsPGN moves = do
  rootDir <- getUserDocumentsDirectory
  createDirectoryIfMissing False $ rootDir </> "XChess"
  dateTime <- getZonedTime
  path <- filepath dateTime (head moves)
  appendFile path $ toPGN moves dateTime

filepath :: ZonedTime -> Move -> IO FilePath
filepath dateTime m = do
  rootDir <- getUserDocumentsDirectory
  return $ rootDir </> "XChess" </> formatTime defaultTimeLocale "%Y-%m-%d" dateTime ++ "_" ++
           formatTime defaultTimeLocale "%H-%M-%S" dateTime ++ "_" ++ nameW m ++ "_vs_" ++ nameB m ++ ".pgn"

toPGN :: [Move] -> ZonedTime -> String
toPGN [] _ = ""
toPGN moves@(m:_) dateTime =
  tagsSection m dateTime ++ "\n\n" ++
  moveSection (if FEN.available $ head moves then tail moves else moves) ++ " " ++ "\n\n"

moveSection :: [Move] -> String
moveSection = unwords . fmap (toString . toSAN) . filter realMove

tagsSection :: Move -> ZonedTime -> String
tagsSection m dateTime =
  "[Event \"?\"]\n\
  \[Site \"?\"]\n\
  \[Date \"" ++ formatTime defaultTimeLocale "%Y.%m.%d" dateTime ++ "\"]\n\
  \[Time \"" ++ formatTime defaultTimeLocale "%T" dateTime ++ "\"]\n\
  \[Round \"?\"]\n\
  \[White \"" ++ Lentils.Api.Move.nameW m ++ "\"]\n\
  \[Black \"" ++ Lentils.Api.Move.nameB m ++ "\"]\n\
  \[Result \"?\"]\n\
  \[BlackElo \"?\"]\n\
  \[WhiteElo \"?\"]\n\
  \[ECO \"?\"]\n\
  \[TimeControl \"?\"]\n\
  \[SetUp \"" ++ (if FEN.available m then "1" else "?") ++ "\"]\n\
  \[FEN \"" ++ (if FEN.available m then FEN.convert m else "?") ++ "\"]\n"

realMove :: Move -> Bool
realMove m = isJust $ movePretty m

toSAN :: Move -> (Int, PColor, String)
toSAN m = (moveNumber m, turn m, fromJust $ movePretty m)

toString :: (Int, PColor, String) -> String
toString (num, Black, move) = show num ++ "." ++ move
toString (_, White, move) = move
