import Data.List

data Fig = W | B | WD | BD | E deriving (Show,Eq)
type Pos = (Int, Int)
type Board = [[Fig]]

charToFig :: Char -> Fig
charToFig 'w' = W
charToFig 'W' = WD
charToFig 'b' = B
charToFig 'B' = BD
charToFig '.' = E

figToChar :: Fig -> Char
figToChar W = 'w'
figToChar WD = 'W'
figToChar B = 'b'
figToChar BD = 'B'
figToChar E = '.'

fromStr :: String -> [Fig]
fromStr str = map charToFig str

toStr :: [Fig] -> String
toStr l = intersperse ' ' (map figToChar l)

initBoard :: String -> Board
initBoard str = map fromStr (lines str)

boardToStr :: Board -> [String]
boardToStr b = map toStr [x | x <- b]

addRowNumber num line = (show num) ++ " " ++ line
addRowNumbers board = zipWith addRowNumber [1..8] board

addColNumbers board = ["  1 2 3 4 5 6 7 8"] ++ board

showBoard :: Board -> String
showBoard board = unlines (addColNumbers (addRowNumbers (boardToStr board)))

getFig :: Board -> Pos -> Fig
getFig board (row, col) = board !! row !! col

replaceWithFig :: Fig -> [Fig] -> Int -> [Fig]
replaceWithFig fig (h:t) 0 = fig : t
replaceWithFig fig (h:t) col = h : replaceWithFig fig t (col - 1)

setFig :: Fig -> Board -> Pos -> Board
setFig fig (h:t) (0, col) = replaceWithFig fig h col : t
setFig fig (h:t) (row, col) = h : setFig fig t ((row - 1), col)

getRow :: Pos -> Int
getRow p = fst p

getCol :: Pos -> Int
getCol p = snd p

isEmpty :: Board -> Pos -> Bool
isEmpty b p = (getFig b p) == E

isWhite :: Board -> Pos -> Bool
isWhite b p = (getFig b p) == W || (getFig b p) == WD

isBlack :: Board -> Pos -> Bool
isBlack b p = (getFig b p) == B || (getFig b p) == BD

isValidPos :: Pos -> Bool
isValidPos (row, col) = (row >= 0) && (row <= 7) && (col >= 0) && (col <= 7)

countIndex :: Int -> Int -> Int
countIndex index neighborIndex = 2 * neighborIndex - index

countPos :: Pos -> Pos -> Pos
countPos (row, col) (neighborRow, neighborCol) = ((countIndex row neighborRow), (countIndex col neighborCol))

--n is a list of nieghbors
getCaptureMoves :: Board -> Pos -> [Pos] -> [Pos]
getCaptureMoves b p n = [pos | pos <- map (countPos p) n, isValidPos pos, isEmpty b pos]

getBlackNeighbors :: Board -> Pos -> [Pos]
getBlackNeighbors b (row, col) = [(x, y) | x <- [(row - 1), (row + 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isBlack b (x, y)]

getWhiteFigComplexMoves :: Board -> Pos -> [Pos]
getWhiteFigComplexMoves b p = getCaptureMoves b p (getBlackNeighbors b p)

getWhiteFigSimpleMoves :: Board -> Pos -> [Pos]
getWhiteFigSimpleMoves b (row, col) = [(x, y) | x <- [(row - 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isEmpty b (x, y)]

getWhiteFigMoves :: Board -> Pos -> [Pos]
getWhiteFigMoves b p = getWhiteFigSimpleMoves b p ++ getWhiteFigComplexMoves b p

getPossibleMoves :: Board -> Pos -> [Pos]
getPossibleMoves b p
	| isEmpty b p = []
	| isWhite b p = getWhiteFigMoves b p
	| isBlack b p = []

b = initBoard ".b.b.b.b\n\
			  \b.b.b.b.\n\
			  \...b.b.b\n\
			  \.b......\n\
			  \..w.....\n\
			  \wb..w.w.\n\
			  \.w.w.w.w\n\
			  \w.w.w.w."

x = showBoard b

list = getPossibleMoves b (4,2)