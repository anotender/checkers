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

addRowNumbers :: [[Char]] -> [[Char]]
addRowNumbers board = zipWith addRowNumber [1..8] board

addColNumbers :: [[Char]] -> [[Char]]
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

countWhiteFigs :: Board -> Int
countWhiteFigs [] = 0
countWhiteFigs (h:t) = (countWhiteFigs t) + (length (filter (\f -> f == W || f == WD) h))

countBlackFigs :: Board -> Int
countBlackFigs [] = 0
countBlackFigs (h:t) = (countBlackFigs t) + (length (filter (\f -> f == B || f == BD) h))

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

capturedPos :: Pos -> Pos -> Pos
capturedPos (r1, c1) (r2, c2) = (quot (r1 + r2) 2, quot (c1 + c2) 2) 

makeWhiteFigCaptureMove :: Board -> Pos -> Pos -> Board
makeWhiteFigCaptureMove b from to = (setFig E (setFig W (setFig E b from) to) (capturedPos from to))

makeBlackFigCaptureMove :: Board -> Pos -> Pos -> Board
makeBlackFigCaptureMove b from to = (setFig E (setFig B (setFig E b from) to) (capturedPos from to))

makeCaptureMove :: Board -> Pos -> Pos -> Board
makeCaptureMove b from to
	| isWhite b from = makeWhiteFigCaptureMove b from to
	| isBlack b from = makeBlackFigCaptureMove b from to

makeSimpleMove :: Board -> Pos -> Pos -> Board
makeSimpleMove b from to
	| isWhite b from = setFig W (setFig E b from) to
	| isBlack b from = setFig B (setFig E b from) to

--n is a list of neighbors
getCaptureMoves :: ((Board, Pos), [Pos]) -> [((Board, Pos), [Pos])]
getCaptureMoves ((_, _), []) = []
getCaptureMoves ((b, p), n) = [((makeCaptureMove b p pos, pos), (getNeighbors (makeCaptureMove b p pos) pos)) | pos <- map (countPos p) n, isValidPos pos, isEmpty b pos]


getBlackNeighbors :: Board -> Pos -> [Pos]
getBlackNeighbors b (row, col) = [(x, y) | x <- [(row - 1), (row + 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isBlack b (x, y)]

getWhiteNeighbors :: Board -> Pos -> [Pos]
getWhiteNeighbors b (row, col) = [(x, y) | x <- [(row - 1), (row + 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isWhite b (x, y)]

getNeighbors :: Board -> Pos -> [Pos]
getNeighbors b p
	| isWhite b p = getBlackNeighbors b p
	| isBlack b p = getWhiteNeighbors b p

getComplexMoves :: Board -> Pos -> [(Board, Pos)]
getComplexMoves b p = map fst (getCaptureMoves ((b, p), (getNeighbors b p)))

getSimpleMoves :: Board -> Pos -> [(Board, Pos)]
getSimpleMoves b (row, col) = [(makeSimpleMove b (row, col) (x, y), (x, y)) | x <- [(row - 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isEmpty b (x, y)]

getMoves :: Board -> Pos -> [(Board, Pos)]
getMoves b p
	| isEmpty b p = []
	| otherwise = getSimpleMoves b p ++ getComplexMoves b p

b = initBoard ".b.b.b.b\n\
			  \b.b.b.b.\n\
			  \...b.b.b\n\
			  \.b......\n\
			  \........\n\
			  \wb..w.w.\n\
			  \ww.w.w.w\n\
			  \w.w.w.w."

x = showBoard b

list = getMoves b (6,0)