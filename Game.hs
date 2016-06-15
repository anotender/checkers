module Game(menu,takeMove,takePos) where

import Data.Maybe
import Data.Ord
import Data.List
import Board
import Move

data Player = WhitePlayer | BlackPlayer deriving (Show, Eq)

menu :: Board -> IO()
menu b = do
	putStr "1. pvp\n2. pve\n3. quit\n> "
	choice <- getLine
	case choice of 
		"1" -> pvp WhitePlayer b
		"2" -> pve WhitePlayer b
		"3" -> return ()
		otherwise -> menu b

pvp :: Player -> Board -> IO()
pvp WhitePlayer b = do
	putStrLn "WhitePlayer move"
	putStr $ showBoard b
	m <- takeMove
	if (isWhite b (fst m)) && (isValidMove b (fst m) (snd m)) 
		then if (countWhiteFigs (fst (extractMove (snd m) (getMoves (b, (fst m)))))) == 0
			then putStrLn "First player won"
			else pvp BlackPlayer (fst (extractMove (snd m) (getMoves (b, (fst m)))))
		else putStrLn "Wrong move" >> pvp WhitePlayer b

pvp BlackPlayer b = do
	putStrLn "BlackPlayer move"
	putStr $ showBoard b
	m <- takeMove
	if (isBlack b (fst m)) && (isValidMove b (fst m) (snd m)) 
		then if (countWhiteFigs (fst (extractMove (snd m) (getMoves (b, (fst m)))))) == 0
			then putStrLn "Second player won"
			else pvp WhitePlayer (fst (extractMove (snd m) (getMoves (b, (fst m)))))
		else putStrLn "Wrong move" >> pvp BlackPlayer b

pve :: Player -> Board -> IO()
pve WhitePlayer b = do
	putStrLn "Player move"
	putStr $ showBoard b
	m <- takeMove
	if (isWhite b (fst m)) && (isValidMove b (fst m) (snd m)) 
		then if (countWhiteFigs (fst (extractMove (snd m) (getMoves (b, (fst m)))))) == 0
			then putStrLn "Player won"
			else pve BlackPlayer (fst (extractMove (snd m) (getMoves (b, (fst m)))))
		else putStrLn "Wrong move" >> pve WhitePlayer b

pve BlackPlayer b = do
	let moves = [getMoves (b, (row, col)) | row <- [0..7], col <- [0..7], isBlack b (row, col)]
	let bestBoard = minimumBy (comparing countWhiteFigs) $ concatMap (map fst) moves
	if countWhiteFigs bestBoard == 0
		then putStrLn "Computer won"
		else pve WhitePlayer bestBoard

takeMove :: IO(Pos, Pos)
takeMove = do
	putStr "from: "
	from <- takePos
	putStr "to: "
	to <- takePos
	return (from, to)

takePos :: IO(Pos)
takePos = do
	pos <- fmap read getLine :: IO Int
	return (quot pos 10, mod pos 10)