module Board(
	Fig(..),
	Board(),
	Pos(), 
	initBoard, 
	showBoard,
	getFig,
	setFig,
	isEmpty, 
	isBlack,
	isWhite,
	isPiece,
	isQueen,
	isValidPos,
	countWhiteFigs,
	countBlackFigs) where

import Data.List

data Fig = W | B | WQ | BQ | E deriving (Show, Eq)

type Board = [[Fig]]
type Pos = (Int, Int)

charToFig :: Char -> Fig
charToFig 'w' = W
charToFig 'W' = WQ
charToFig 'b' = B
charToFig 'B' = BQ
charToFig '.' = E

figToChar :: Fig -> Char
figToChar W = 'w'
figToChar WQ = 'W'
figToChar B = 'b'
figToChar BQ = 'B'
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
countWhiteFigs b = foldl (\acc x -> if (x == W || x == WQ) then acc + 1 else acc) 0 (concat b)

countBlackFigs :: Board -> Int
countBlackFigs b = foldl (\acc x -> if (x == B || x == BQ) then acc + 1 else acc) 0 (concat b)

isEmpty :: Board -> Pos -> Bool
isEmpty b p = (getFig b p) == E

isWhite :: Board -> Pos -> Bool
isWhite b p = f == W || f == WQ where f = getFig b p

isBlack :: Board -> Pos -> Bool
isBlack b p = f == B || f == BQ where f = getFig b p

isPiece :: Board -> Pos -> Bool
isPiece b p = f == B || f == W where f = getFig b p

isQueen :: Board -> Pos -> Bool
isQueen b p = f == BQ || f == WQ where f = getFig b p

isValidPos :: Pos -> Bool
isValidPos (row, col) = (row >= 0) && (row <= 7) && (col >= 0) && (col <= 7)