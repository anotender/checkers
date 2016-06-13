module Move(
	Move(),
	Moves(),
	getMoves,
	isValidMove,
	extractMove) where

import Data.List
import Data.Maybe
import Board

type Neighbor = Pos
type Neighbors = [Neighbor]
type Move = (Board, Pos)
type Moves = [Move]

isEmptyLine :: Board -> Pos -> Pos -> Bool
isEmptyLine b p1 p2 = all (isEmpty b) (createLine b p1 p2)

createLine :: Board -> Pos -> Pos -> [Pos]
createLine b (r1, c1) (r2, c2) = [(x, y) | x <- [(min r1 r2)..(max r1 r2)], y <- [(min c1 c2)..(max c1 c2)], x /= r1, y /= c1, r1 - c1 == x - y || r1 + c1 == x + y]

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

makeQueenCaptureMove :: Board -> Pos -> Pos -> Board
makeQueenCaptureMove b from to = 
	(setFig E (setFig f (setFig E b from) to) capturedPos)
	where
		f = getFig b from
		capturedPos = head (filter (\p -> containsInTheLine p from to) (getNeighbors b from))
		containsInTheLine (r, c) (fr, fc) (tr, tc) = r < (max fr tr) && r > (min fr tr) && c < (max fc tc) && c > (min fc tc)

getQueenCaptureMoves :: (Move, Neighbors) -> [(Move, Neighbors)]
getQueenCaptureMoves (_, []) = []
getQueenCaptureMoves ((b, p), n) = 
	x ++ concatMap getQueenCaptureMoves x
	where
		x = [((makeQueenCaptureMove b p pos, pos), (getNeighbors (makeQueenCaptureMove b p pos) pos)) | pos <- map (countPos p) n, isValidPos pos, isEmpty b pos]

getNeighbors :: Board -> Pos -> Neighbors
getNeighbors b (row, col)
	| isEmpty b (row, col) = []
	| isPiece b (row, col) && isWhite b (row, col) = [(x, y) | x <- [(row - 1), (row + 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isBlack b (x, y)]
	| isPiece b (row, col) && isBlack b (row, col) = [(x, y) | x <- [(row - 1), (row + 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isWhite b (x, y)]
	| isQueen b (row, col) && isWhite b (row, col) = filter (isBlack b) [(x, y) | x <- [0..7], y <- [0..7], row - col == x - y || row + col == x + y, hasOnlyOneBlackEnemy (createLine b (row, col) (x, y))]
	| isQueen b (row, col) && isBlack b (row, col) = filter (isWhite b) [(x, y) | x <- [0..7], y <- [0..7], row - col == x - y || row + col == x + y, hasOnlyOneWhiteEnemy (createLine b (row, col) (x, y))]
	where
		hasOnlyOneBlackEnemy l = length (filter (isBlack b) l) == 1
		hasOnlyOneWhiteEnemy l = length (filter (isWhite b) l) == 1

promote :: Board -> Board
promote b = 
	promoteBlack (promoteWhite b)
	where 
		promoteWhite b = [foldl (\acc x -> if (x == W) then acc ++ [WQ] else acc ++ [x]) [] (head b)] ++ (tail b)
		promoteBlack b = (init b) ++ [foldl (\acc x -> if (x == B) then acc ++ [BQ] else acc ++ [x]) [] (last b)]

getPieceComplexMoves :: Board -> Pos -> Moves
getPieceComplexMoves b p = map fst (getPieceCaptureMoves ((b, p), (getNeighbors b p)))

getPieceSimpleMoves :: Board -> Pos -> Moves
getPieceSimpleMoves b (row, col) = [(makeSimpleMove b (row, col) (x, y), (x, y)) | x <- [if isWhite b (row, col) then (row - 1) else (row + 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isEmpty b (x, y)]

getQueenComplexMoves :: Board -> Pos -> Moves
getQueenComplexMoves b p = map fst (getQueenCaptureMoves ((b, p), (getNeighbors b p)))

getQueenSimpleMoves :: Board -> Pos -> Moves
getQueenSimpleMoves b (row, col) = [(makeSimpleMove b (row, col) (x, y), (x, y)) | x <- [0..7], y <- [0..7], row - col == x - y || row + col == x + y, isEmptyLine b (row, col) (x, y)]

getMoves :: Board -> Pos -> Moves
getMoves b p
	| isEmpty b p = []
	| isPiece b p = zip (map promote (map fst (getPieceSimpleMoves b p ++ getPieceComplexMoves b p))) (map snd (getPieceSimpleMoves b p ++ getPieceComplexMoves b p))
	| isQueen b p = getQueenSimpleMoves b p ++ getQueenComplexMoves b p

extractMove :: Pos -> Moves -> Move
extractMove p m = m !! fromJust (elemIndex p $ map snd m)