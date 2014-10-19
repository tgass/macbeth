module Main where

import Graphics.UI.WX

data Column = A | B | C | D | E | F | G | H deriving (Show, Enum)
data Row = ONE | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT deriving (Show, Enum)
data Color_ = BLACK | WHITE deriving (Show)
data Piece = PAWN | ROOK | KNIGHT | BISHOP | QUEEN | KING deriving (Show)

type Field = (Column, Row)
type Board = [(Field, Piece, Color_)]

main :: IO ()
main = start chessBoardFrame

chessBoardFrame :: IO ()
chessBoardFrame = do
       board <- variable [ value := initBoard ]
       selectedField <- variable [ value := (A, ONE) ]
       f <- frame [ text := "XChess"
                   ,on paint := drawAll
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

drawAll dc view = do
  drawBoard dc view
  mapM_ (drawPiece dc view) initBoard

drawPiece dc view (f, p, c) = drawBitmap dc (piece p c) (toPos f) True []

drawBoard dc view = drawBitmap dc board (point 0 0) False []

toField :: Point -> Field
toField p = (intToCol ((pointX p) `div` 40), intToRow((pointY p) `div` 40))

toPos :: Field -> Point
toPos (c, r) = point ((colToInt c) * 40) ((rowToInt r) * 40)

rowToInt :: Row -> Int
rowToInt =  abs . (7-) . fromEnum

intToRow :: Int -> Row
intToRow = toEnum . abs . (7-)

intToCol :: Int -> Column
intToCol = toEnum

colToInt :: Column -> Int
colToInt = fromEnum

board = bitmap $ root ++ "board_empty.gif"

piece KING BLACK  = bitmap $ root ++ "bk.gif"
piece QUEEN BLACK  = bitmap $ root ++ "bq.gif"
piece ROOK BLACK  = bitmap $ root ++ "br.gif"
piece KNIGHT BLACK  = bitmap $ root ++ "bn.gif"
piece BISHOP BLACK  = bitmap $ root ++ "bb.gif"
piece PAWN BLACK  = bitmap $ root ++ "bp.gif"
piece KING WHITE  = bitmap $ root ++ "wk.gif"
piece QUEEN WHITE  = bitmap $ root ++ "wq.gif"
piece ROOK WHITE  = bitmap $ root ++ "wr.gif"
piece KNIGHT WHITE  = bitmap $ root ++ "wn.gif"
piece BISHOP WHITE  = bitmap $ root ++ "wb.gif"
piece PAWN WHITE  = bitmap $ root ++ "wp.gif"

root = "/Users/tilmann/Documents/leksah/ChessGui/gif/"

initBoard :: Board
initBoard = [((A, ONE), ROOK, WHITE),
             ((B, ONE), KNIGHT, WHITE),
             ((C, ONE), BISHOP, WHITE),
             ((D, ONE), QUEEN, WHITE),
             ((E, ONE), KING, WHITE),
             ((F, ONE), BISHOP, WHITE),
             ((G, ONE), KNIGHT, WHITE),
             ((H, ONE), ROOK, WHITE),
             ((A, TWO), PAWN, WHITE),
             ((B, TWO), PAWN, WHITE),
             ((C, TWO), PAWN, WHITE),
             ((D, TWO), PAWN, WHITE),
             ((E, TWO), PAWN, WHITE),
             ((F, TWO), PAWN, WHITE),
             ((G, TWO), PAWN, WHITE),
             ((H, TWO), PAWN, WHITE),

             ((A, EIGHT), ROOK, BLACK),
             ((B, EIGHT), KNIGHT, BLACK),
             ((C, EIGHT), BISHOP, BLACK),
             ((D, EIGHT), QUEEN, BLACK),
             ((E, EIGHT), KING, BLACK),
             ((F, EIGHT), BISHOP, BLACK),
             ((G, EIGHT), KNIGHT, BLACK),
             ((H, EIGHT), ROOK, BLACK),
             ((A, SEVEN), PAWN, BLACK),
             ((B, SEVEN), PAWN, BLACK),
             ((C, SEVEN), PAWN, BLACK),
             ((D, SEVEN), PAWN, BLACK),
             ((E, SEVEN), PAWN, BLACK),
             ((F, SEVEN), PAWN, BLACK),
             ((G, SEVEN), PAWN, BLACK),
             ((H, SEVEN), PAWN, BLACK)]
