module PGN (
  saveAsPGN
) where

import Api
import Move
import Game

import Data.Maybe
import Data.List
import Data.Time
import System.Directory
import System.FilePath


saveAsPGN :: [Move] -> String -> GameResult -> IO ()
saveAsPGN moves playerName result = do
  rootDir <- getUserDocumentsDirectory
  createDirectoryIfMissing False $ rootDir </> "XChess"
  date <- fmap (formatDate . toGregorian . utctDay) getCurrentTime
  appendFile (rootDir </> "XChess" </> (playerName ++ ".pgn")) $ toPGN moves result date

toPGN :: [Move] -> GameResult -> String -> String
toPGN [] _ _ = ""
toPGN moves@(m:mx) result date = tagsSection m date ++ "\n\n" ++ moveSection moves ++ " " ++ (show result) ++ "\n\n"

moveSection :: [Move] -> String
moveSection = unwords . fmap (toString . toSAN) . filter realMove

tagsSection :: Move -> String -> String
tagsSection m date =
  "[Event \"?\"]\n\
  \[Site \"?\"]\n\
  \[Date \"" ++ date ++ "\"]\n\
  \[Round \"?\"]\n\
  \[White \"" ++ Move.nameW m ++ "\"]\n\
  \[Black \"" ++ Move.nameB m ++ "\"]\n\
  \[Result \"?\"]\n\
  \[BlackElo \"?\"]\n\
  \[WhiteElo \"?\"]\n\
  \[ECO \"?\"]\n\
  \[TimeControl \"?\"]"

formatDate :: (Integer, Int, Int) -> String
formatDate (year, month, day) = show year ++ "." ++ show month ++ "." ++ show day

realMove :: Move -> Bool
realMove m = isJust $ movePretty m

toSAN :: Move -> (Int, PColor, String)
toSAN m = (moveNumber m, turn m, fromJust $ movePretty m)

toString :: (Int, PColor, String) -> String
toString (num, Black, move) = show num ++ "." ++ move
toString (_, White, move) = move
