import Data.List
import Data.Tree
import Data.Maybe

data Fig = W | B | WK | BK | E deriving (Show, Eq)
data Player = WhitePlayer | BlackPlayer deriving (Show, Eq)
type Pos = (Int, Int)
type Neighbor = Pos
type Neighbors = [Neighbor]
type Move = (Board, Pos)
type Moves = [Move]
type Board = [[Fig]]

charToFig :: Char -> Fig
charToFig 'w' = W
charToFig 'W' = WK
charToFig 'b' = B
charToFig 'B' = BK
charToFig '.' = E

figToChar :: Fig -> Char
figToChar W = 'w'
figToChar WK = 'W'
figToChar B = 'b'
figToChar BK = 'B'
figToChar E = '.'

fromStr :: String -> [Fig]
fromStr str = map charToFig str

toStr :: [Fig] -> String
toStr l = intersperse ' ' $ map figToChar l

initBoard :: String -> Board
initBoard str = map fromStr $ lines str

boardToStr :: Board -> [String]
boardToStr b = map toStr b

addRowNumber :: (Show a) => a -> String -> String
addRowNumber num line = (show num) ++ " " ++ line

addRowNumbers :: [String] -> [String]
addRowNumbers b = zipWith addRowNumber [0..7] b

addColNumbers :: [String] -> [String]
addColNumbers b = ["  0 1 2 3 4 5 6 7"] ++ b

showBoard :: Board -> String
showBoard b = unlines $ addColNumbers $ addRowNumbers $ map toStr b

getFig :: Board -> Pos -> Fig
getFig b (row, col) = b !! row !! col

replaceWithFig :: Fig -> [Fig] -> Int -> [Fig]
replaceWithFig fig (h:t) 0 = fig : t
replaceWithFig fig (h:t) col = h : replaceWithFig fig t (col - 1)

setFig :: Fig -> Board -> Pos -> Board
setFig fig (h:t) (0, col) = replaceWithFig fig h col : t
setFig fig (h:t) (row, col) = h : setFig fig t ((row - 1), col)

countWhiteFigs :: Board -> Int
countWhiteFigs b = foldl (\acc x -> if (x == W || x == WK) then acc + 1 else acc) 0 (concat b)

countBlackFigs :: Board -> Int
countBlackFigs b = foldl (\acc x -> if (x == B || x == BK) then acc + 1 else acc) 0 (concat b)

isEmptyLine :: Board -> Pos -> Pos -> Bool
isEmptyLine b p1 p2 = all (isEmpty b) (createLine b p1 p2)

createLine :: Board -> Pos -> Pos -> [Pos]
createLine b (r1, c1) (r2, c2) = [(x, y) | x <- [(min r1 r2)..(max r1 r2)], y <- [(min c1 c2)..(max c1 c2)], x /= r1, y /= c1, r1 - c1 == x - y || r1 + c1 == x + y]

isEmpty :: Board -> Pos -> Bool
isEmpty b p = (getFig b p) == E

isWhite :: Board -> Pos -> Bool
isWhite b p = f == W || f == WK where f = getFig b p

isBlack :: Board -> Pos -> Bool
isBlack b p = f == B || f == BK where f = getFig b p

isPiece :: Board -> Pos -> Bool
isPiece b p = f == B || f == W where f = getFig b p

isKing :: Board -> Pos -> Bool
isKing b p = f == BK || f == WK where f = getFig b p

isValidPos :: Pos -> Bool
isValidPos (row, col) = (row >= 0) && (row <= 7) && (col >= 0) && (col <= 7)

isValidMove :: Board -> Pos -> Pos -> Bool
isValidMove b from to = (isValidPos to) && (elem to (map snd $ getMoves b from))

countPos :: Pos -> Pos -> Pos
countPos (row, col) (neighborRow, neighborCol) = 
	((countIndex row neighborRow), (countIndex col neighborCol))
	where
		countIndex index neighborIndex = neighborIndex + signum (neighborIndex - index)

makeSimpleMove :: Board -> Pos -> Pos -> Board
makeSimpleMove b from to = setFig f (setFig E b from) to where f = getFig b from

makePieceCaptureMove :: Board -> Pos -> Pos -> Board
makePieceCaptureMove b from to = 
	(setFig E (setFig f (setFig E b from) to) (capturedPos from to))
	where
		f = getFig b from
		capturedPos (r1, c1) (r2, c2) = (quot (r1 + r2) 2, quot (c1 + c2) 2)

getPieceCaptureMoves :: (Move, Neighbors) -> [(Move, Neighbors)]
getPieceCaptureMoves (_, []) = []
getPieceCaptureMoves ((b, p), n) = 
	x ++ concatMap getPieceCaptureMoves x
	where
		x = [((makePieceCaptureMove b p pos, pos), (getNeighbors (makePieceCaptureMove b p pos) pos)) | pos <- map (countPos p) n, isValidPos pos, isEmpty b pos]

makeKingCaptureMove :: Board -> Pos -> Pos -> Board
makeKingCaptureMove b from to = 
	(setFig E (setFig f (setFig E b from) to) capturedPos)
	where
		f = getFig b from
		capturedPos = head (filter (\p -> containsInTheLine p from to) (getNeighbors b from))
		containsInTheLine (r, c) (fr, fc) (tr, tc) = r < (max fr tr) && r > (min fr tr) && c < (max fc tc) && c > (min fc tc)

getKingCaptureMoves :: (Move, Neighbors) -> [(Move, Neighbors)]
getKingCaptureMoves (_, []) = []
getKingCaptureMoves ((b, p), n) = 
	x ++ concatMap getKingCaptureMoves x
	where
		x = [((makeKingCaptureMove b p pos, pos), (getNeighbors (makeKingCaptureMove b p pos) pos)) | pos <- map (countPos p) n, isValidPos pos, isEmpty b pos]

getNeighbors :: Board -> Pos -> Neighbors
getNeighbors b (row, col)
	| isEmpty b (row, col) = []
	| isPiece b (row, col) && isWhite b (row, col) = [(x, y) | x <- [(row - 1), (row + 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isBlack b (x, y)]
	| isPiece b (row, col) && isBlack b (row, col) = [(x, y) | x <- [(row - 1), (row + 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isWhite b (x, y)]
	| isKing b (row, col) && isWhite b (row, col) = filter (isBlack b) [(x, y) | x <- [0..7], y <- [0..7], row - col == x - y || row + col == x + y, hasOnlyWhitePlayerBlackPlayerEnemy (createLine b (row, col) (x, y))]
	| isKing b (row, col) && isBlack b (row, col) = filter (isWhite b) [(x, y) | x <- [0..7], y <- [0..7], row - col == x - y || row + col == x + y, hasOnlyWhitePlayerWhitePlayerEnemy (createLine b (row, col) (x, y))]
	where
		hasOnlyWhitePlayerBlackPlayerEnemy l = length (filter (isBlack b) l) == 1
		hasOnlyWhitePlayerWhitePlayerEnemy l = length (filter (isWhite b) l) == 1

getPieceComplexMoves :: Board -> Pos -> Moves
getPieceComplexMoves b p = map fst (getPieceCaptureMoves ((b, p), (getNeighbors b p)))

getPieceSimpleMoves :: Board -> Pos -> Moves
getPieceSimpleMoves b (row, col) = [(makeSimpleMove b (row, col) (x, y), (x, y)) | x <- [if isWhite b (row, col) then (row - 1) else (row + 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isEmpty b (x, y)]

getKingComplexMoves :: Board -> Pos -> Moves
getKingComplexMoves b p = map fst (getKingCaptureMoves ((b, p), (getNeighbors b p)))

getKingSimpleMoves :: Board -> Pos -> Moves
getKingSimpleMoves b (row, col) = [(makeSimpleMove b (row, col) (x, y), (x, y)) | x <- [0..7], y <- [0..7], row - col == x - y || row + col == x + y, isEmptyLine b (row, col) (x, y)]

getMoves :: Board -> Pos -> Moves
getMoves b p
	| isEmpty b p = []
	| isPiece b p = getPieceSimpleMoves b p ++ getPieceComplexMoves b p
	| isKing b p = getKingSimpleMoves b p ++ getKingComplexMoves b p

extractMove :: Pos -> Moves -> Move
extractMove p m = m !! fromJust (elemIndex p $ map snd m)

initialBoardStr = ".b.b.b.b\n\
				  \b.b.b.b.\n\
				  \.b.b.b.b\n\
				  \........\n\
				  \........\n\
				  \w.w.w.w.\n\
				  \.w.w.w.w\n\
				  \w.w.w.w."

initialBoard = initBoard initialBoardStr

pvp WhitePlayer b = do
	putStrLn "First player move"
	putStr $ showBoard b
	m <- takeMove
	if (isWhite b (fst m)) && (isValidMove b (fst m) (snd m)) 
		then if (countWhiteFigs (fst (extractMove (snd m) (getMoves b (fst m))))) == 0
			then putStrLn "First player won"
			else pvp BlackPlayer (fst (extractMove (snd m) (getMoves b (fst m))))
		else putStrLn "Wrong move" >> pvp WhitePlayer b

pvp BlackPlayer b = do
	putStrLn "Second player move"
	putStr $ showBoard b
	m <- takeMove
	if (isBlack b (fst m)) && (isValidMove b (fst m) (snd m)) 
		then if (countWhiteFigs (fst (extractMove (snd m) (getMoves b (fst m))))) == 0
			then putStrLn "Second player won"
			else pvp WhitePlayer (fst (extractMove (snd m) (getMoves b (fst m))))
		else putStrLn "Wrong move" >> pvp BlackPlayer b

pve = do
	putStrLn "TODO"

takeMove = do
	putStr "from: "
	from <- takePos
	putStr "to: "
	to <- takePos
	return (from, to)

takePos = do
	pos <- fmap read getLine :: IO Int
	return (quot pos 10, mod pos 10)