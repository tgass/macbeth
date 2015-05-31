module PGN (
  saveAsPGN
) where

import Api
import Move
import Game

import Data.Maybe
import Data.List
import System.Directory
import System.FilePath

--TODO: Saving in different files depending guest or not
saveAsPGN :: [Move] -> GameResult -> IO ()
saveAsPGN moves result = do
  rootDir <- getUserDocumentsDirectory
  createDirectoryIfMissing False $ rootDir </> "XChess"
  appendFile (rootDir </> "XChess" </> "games.pgn") $ toPGN moves result

toPGN :: [Move] -> GameResult -> String
toPGN [] _ = ""
toPGN moves@(m:mx) result = tagsSection m ++ "\n\n" ++ moveSection moves ++ " " ++ (show result) ++ "\n\n"

moveSection :: [Move] -> String
moveSection = unwords . fmap (toString . toSAN) . filter realMove

tagsSection :: Move -> String
tagsSection m =
  "[event \"?\"]\n\
  \[site \"?\"]\n\
  \[date \"?\"]\n\
  \[Round \"?\"]\n\
  \[White \"" ++ Move.nameW m ++ "\"]\n\
  \[Black \"" ++ Move.nameB m ++ "\"]\n\
  \[Result \"?\"]\n\
  \[BlackElo \"?\"]\n\
  \[WhiteElo \"?\"]\n\
  \[ECO \"?\"]\n\
  \[TimeControl \"?\"]"

realMove :: Move -> Bool
realMove m = isJust $ movePretty m

toSAN :: Move -> (Int, PColor, String)
toSAN m = (moveNumber m, turn m, fromJust $ movePretty m)

toString :: (Int, PColor, String) -> String
toString (num, Black, move) = show num ++ "." ++ move
toString (_, White, move) = move
