module Main where

import Game
import Board

main = menu initialBoard

initialBoardStr = ".b.b.b.b\n\
				  \b.b.b.b.\n\
				  \.b.b.b.b\n\
				  \........\n\
				  \........\n\
				  \w.w.w.w.\n\
				  \.w.w.w.w\n\
				  \w.w.w.w."

initialBoard = initBoard initialBoardStr