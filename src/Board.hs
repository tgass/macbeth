module Board (
  --main,
  drawAll,
  paintSelectedSquare,
  setSelectedField
) where

import Seek
import Graphics.UI.WX

import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)

main :: IO ()
main = start chessBoardFrame

chessBoardFrame :: IO ()
chessBoardFrame = do
       selSq <- variable [ value := (Square A One) ]
       position <- variable [value := initBoard]
       f <- frame [ text := "XChess"
                   ,on paint := drawAll position
                   ]
       p <- panel f [ on paint := paintSelectedSquare selSq
                    ]
       p' <- panel p [ on mouse := setSelectedField selSq p ]
       set f [layout := space 320 320]
       set p [layout := space 320 320]
       set p' [layout := space 320 320]

paintSelectedSquare selSq dc view = do
                  val <- get selSq value
                  let pointX' = pointX $ toPos val
                      pointY' = pointY $ toPos val
                  set dc [penColor := rgb 255 0 (0 :: Int) ]
                  drawRect dc (Rect pointX' pointY' (40+1) (40+1)) []

setSelectedField selSq p mouse = case mouse of
                                 MouseMotion pt mod -> set selSq [ value :~ \x -> toField pt] >> repaint p
                                 otherwise -> return ()

drawAll position dc view = drawBoard dc view >>
                           get position value >>= \pos ->
                           mapM_ (drawPiece dc view) pos
                           where drawPiece dc view (square, p) = drawBitmap dc (piece p) (toPos square) True []
                                 drawBoard dc view = drawBitmap dc board (point 0 0) False []

toField :: Point -> Square
toField p = Square (intToCol ((pointX p) `div` 40)) (intToRow((pointY p) `div` 40))

toPos :: Square -> Point
toPos (Square c r) = point ((colToInt c) * 40) ((rowToInt r) * 40)

rowToInt :: Row -> Int
rowToInt =  abs . (7-) . fromEnum

colToInt :: Column -> Int
colToInt = fromEnum

intToRow :: Int -> Row
intToRow = toEnum . abs . (7-)

intToCol :: Int -> Column
intToCol = toEnum

board = bitmap $ root ++ "board_empty.gif"

piece (Piece King Black) = bitmap $ root ++ "bk.gif"
piece (Piece Queen Black) = bitmap $ root ++ "bq.gif"
piece (Piece Rook Black) = bitmap $ root ++ "br.gif"
piece (Piece Knight Black) = bitmap $ root ++ "bn.gif"
piece (Piece Bishop Black) = bitmap $ root ++ "bb.gif"
piece (Piece Pawn Black) = bitmap $ root ++ "bp.gif"
piece (Piece King White) = bitmap $ root ++ "wk.gif"
piece (Piece Queen White) = bitmap $ root ++ "wq.gif"
piece (Piece Rook White) = bitmap $ root ++ "wr.gif"
piece (Piece Knight White) = bitmap $ root ++ "wn.gif"
piece (Piece Bishop White) = bitmap $ root ++ "wb.gif"
piece (Piece Pawn White) = bitmap $ root ++ "wp.gif"

root = "/Users/tilmann/Documents/leksah/XChess/gif/"

initBoard :: Position
initBoard = []



