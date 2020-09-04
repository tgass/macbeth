module Macbeth.Wx.Game.PieceSet (
  PieceSet(..),
  display,
  findSize,
  initPieceBitmaps
) where

import           Control.Monad.List
import           Control.Applicative
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Aeson.Types hiding (Parser)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 hiding (dropWhile)
import           Data.Maybe
import           GHC.Generics
import           Graphics.UI.WX hiding (size)
import           Macbeth.Fics.Api.Api
import qualified Paths as Paths
import           Safe
import           System.Directory
import           System.FilePath

{-
http://ixian.com/chess/jin-piece-sets/
This work by Eric De Mund is licensed under a Creative Commons Attribution-Share Alike 3.0 Unported License
-}

data PieceSet = Alpha1 | Alpha2 | Merida1 | Merida2 | Uscf1 | Uscf2 deriving (Show, Eq, Ord, Enum, Generic)

initPieceBitmaps :: IO (Map (PieceSet, Piece, Int) (Bitmap ()))
initPieceBitmaps = fmap Map.fromList $ runListT loadImages

loadImages :: ListT IO ((PieceSet, Piece, Int), Bitmap ())
loadImages = do
  piecesDir <- ListT $ fmap pure $ Paths.getPiecesDir
  file <- ListT $ listDirectory piecesDir
  case parseOnly filenameParser (pack file) of
    Left _ -> mzero
    Right description -> return (description, bitmap $ piecesDir </> file)

filenameParser :: Parser (PieceSet, Piece, Int)
filenameParser = do
  pieceSet <- pieceSetParser <* "_"
  size <- decimal <* "_"
  piece <- pieceParser <* "."
  return (pieceSet, piece, size)

pieceSetParser :: Parser PieceSet
pieceSetParser = do
      "alpha-ead-01" *> pure Alpha1
  <|> "alpha-ead-02" *> pure Alpha2
  <|> "merida-ead-01" *> pure Merida1
  <|> "merida-ead-02" *> pure Merida2
  <|> "uscf-ead-01" *> pure Uscf1
  <|> "uscf-ead-02" *> pure Uscf2

pieceParser :: Parser Piece
pieceParser = 
      "bk" *> pure (Piece King Black)
  <|> "bq" *> pure (Piece Queen Black)
  <|> "br" *> pure (Piece Rook Black)
  <|> "bn" *> pure (Piece Knight Black)
  <|> "bb" *> pure (Piece Bishop Black)
  <|> "bp" *> pure (Piece Pawn Black)
  <|> "wk" *> pure (Piece King White)
  <|> "wq" *> pure (Piece Queen White)
  <|> "wr" *> pure (Piece Rook White)
  <|> "wn" *> pure (Piece Knight White)
  <|> "wb" *> pure (Piece Bishop White)
  <|> "wp" *> pure (Piece Pawn White)


display :: PieceSet -> String
display Alpha1 = "Alpha (ead-01)"
display Alpha2 = "Alpha (ead-02)"
display Merida1 = "Merida (ead-01)"
display Merida2 = "Merida (ead-02)"
display Uscf1 = "USCF (ead-01)"
display Uscf2 = "USCF (ead-02)"


sizes :: [Int]
sizes = [20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,52,56,60,64,72,80,88,96,112,128,144,300]

-- returns piece size and scale
findSize :: Int -> (Int, Double)
findSize panelWidth =
  let psize = fromMaybe 300 $ headMay $ dropWhile (< round (fromIntegral panelWidth / (8 :: Double))) sizes
  in (psize, fromIntegral panelWidth / (8 :: Double) / fromIntegral psize)

instance ToJSON PieceSet
instance FromJSON PieceSet

