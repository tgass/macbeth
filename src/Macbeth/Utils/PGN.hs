module Macbeth.Utils.PGN (
  saveAsPGN
) where

import Macbeth.Fics.Configuration
import Macbeth.Fics.Api.Api
import Macbeth.Fics.Api.Move
import Macbeth.Fics.Api.Game (GameResult)
import qualified Macbeth.Utils.FEN as FEN
import Macbeth.Wx.BoardState

import Control.Monad
import Data.Maybe
import Data.Time
import System.FilePath


saveAsPGN :: BoardState ->  IO ()
saveAsPGN b = saveAsPGN' (reverse $ moves b) (gameResult b)

saveAsPGN' :: [Move] -> Maybe GameResult -> IO ()
saveAsPGN' [] _ = return ()
saveAsPGN' moves mGameResult = do
  dateTime <- getZonedTime
  appDir <- directory `fmap` loadConfig
  when (isJust appDir) $ do
    path <- filepath (fromJust appDir) dateTime (head moves)
    appendFile path $ toPGN (filter (isJust . movePretty) moves) mGameResult dateTime

filepath :: FilePath -> ZonedTime -> Move -> IO FilePath
filepath appDir dateTime m = return $ appDir </>
  formatTime Data.Time.defaultTimeLocale "%Y-%m-%d_%H-%M-%S_" dateTime ++ nameW m ++ "_vs_" ++ nameB m ++ ".pgn"

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
  \[Date \"" ++ formatTime Data.Time.defaultTimeLocale "%Y.%m.%d" dateTime ++ "\"]\n\
  \[Time \"" ++ formatTime Data.Time.defaultTimeLocale "%T" dateTime ++ "\"]\n\
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
