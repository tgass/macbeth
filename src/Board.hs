module Board (
  drawAll
) where

import Seek
import Graphics.UI.WX

main :: IO ()
main = start chessBoardFrame

chessBoardFrame :: IO ()
chessBoardFrame = do
       selectedField <- variable [ value := (Square A One) ]
       position <- variable [ value := []]
       f <- frame [ text := "XChess"
                   ,on paint := drawAll position
                   ]
       p <- panel f [ on paint := paintPanel selectedField
                    ]
       p' <- panel p [ on mouse := onMouse selectedField p ]
       set f [layout := space 320 320]
       set p [layout := space 320 320]
       set p' [layout := space 320 320]

paintPanel field dc view = do
                  f <- get field value
                  let pointX' = pointX $ (toPos f)
                      pointY' = pointY $ (toPos f)
                  set dc [penColor := rgb 255 0 (0 :: Int) ]
                  drawRect dc (Rect pointX' pointY' (40+1) (40+1)) []

onMouse f p mouse = case mouse of
                   MouseMotion pt mod -> do let f' = toField pt
                                            set f [ value :~ \x -> toField pt]
                                            repaint p
                   MouseLeftDown pt mod -> putStr $ show $ toField pt
                   otherwise -> return ()

drawAll position dc view = get position value >>= \p -> drawBoard dc view >> mapM_ (drawPiece dc view) p
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

board' = "-----rk- pp-bb-qp ----p--- ---pP-p- ---PP-Q- --PB---- P--B---P -----RK-"

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

initBoard :: [(Square, Piece)]
initBoard = parsePosition "-----rk- pp-bb-qp ----p--- ---pP-p- ---PP-Q- --PB---- P--B---P -----RK-"




