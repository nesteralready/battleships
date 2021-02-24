module Coordinate where

import Data.Char (ord)

type Coordinate = (Int, Int)

data Cell = Hit Coordinate | Miss Coordinate 


stringToCoord :: String -> Coordinate
stringToCoord (a:x:b:y:c) = ((ord x) - (ord '0') + 1, (ord y) - (ord '0') + 1 )
stringToCoord _ = (-1, -1)

getCoordsFromInput :: String -> [String]
getCoordsFromInput [] = [[]]
getCoordsFromInput (x:xs) = if x == ';' then
                                      [] : getCoordsFromInput xs
                                  else                           
                                      (x : head (getCoordsFromInput xs)) : tail (getCoordsFromInput xs)

checkValidCoord :: Coordinate -> Bool
checkValidCoord coord = fst coord >= 1 && snd coord >= 1 && fst coord <= 10 && snd coord <= 10
                               