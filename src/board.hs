module Board where

import Data.List
import Coordinate 
import Ship

type Board = [[Bool]]

startBoard = [[False,False,False,False,False,False,False,False,False,False],
                  [False,False,False,False,False,False,False,False,False,False],
                  [False,False,False,False,False,False,False,False,False,False],
                  [False,False,False,False,False,False,False,False,False,False],
                  [False,False,False,False,False,False,False,False,False,False],
                  [False,False,False,False,False,False,False,False,False,False],
                  [False,False,False,False,False,False,False,False,False,False],
                  [False,False,False,False,False,False,False,False,False,False],
                  [False,False,False,False,False,False,False,False,False,False],
                  [False,False,False,False,False,False,False,False,False,False]]

printBoard :: String -> Board -> [Ship] -> IO ()
printBoard playerName field ships = do
                                      putStrLn (playerName ++ "'s field:")
                                      putStrLn (take (12) (repeat '-') ++ "\n-" ++ boardToString field ships (1, 1) ++ take (11) (repeat '-') )
                                      putStrLn ""

boardToString :: Board -> [Ship] -> Coordinate -> String
boardToString field ships coordinate
        | fst coordinate <= 10
        
          && snd coordinate <= 10 = if (field !! ((snd coordinate) - 1)) !! ((fst coordinate) - 1) == True then
                                                if or [coordinate == coord | ship <- ships, coord <- ship] then 'o' : boardToString field ships (fst coordinate + 1, snd coordinate)
                                                  else 'x' : boardToString field ships (fst coordinate + 1, snd coordinate)
                                        else ' ' : boardToString field ships (fst coordinate + 1, snd coordinate)
        
        | snd coordinate <= 10= "-\n-" ++ boardToString field ships (1, snd coordinate + 1)
        | otherwise = []


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

-- printBoard :: Board -> Turn -> IO()
-- printBoard [] Mine = print "YOUR BOARD"
-- printBoard [] Opponent = print "OPPONENT BOARD"
-- printBoard board turn = do
--         print (addSpaces(head board))
--         printBoard (tail board) turn

-- getBoard :: Board -> Board
-- getBoard b = b


-- addSpaces :: String -> String
-- addSpaces xs@[_] = xs
-- addSpaces    (x:xs) = x:' ':addSpaces xs
-- addSpaces xs              = xs

-- boardToString :: Board -> String
-- boardToString = concat

-- stringToBoard :: String -> Board
-- stringToBoard "" = []
-- stringToBoard (a:b:c:d:e:f:g:h:i:j:str) = [a,b,c,d,e,f,g,h,i,j] : stringToBoard str
