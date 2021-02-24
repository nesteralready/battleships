module Ship where

import Coordinate

type Ship = [Coordinate]

minShipSize:: Int
minShipSize = 2

maxShipSize:: Int
maxShipSize = 5

getShip :: [Ship] -> Int -> IO Ship
getShip placedShips len = do
                              putStrLn ("Enter the coordinates of the ship of length " ++ show len ++ "!")
                              string <- getLine
                              let stringCoords = getCoordsFromInput string
                              let coords = map stringToCoord stringCoords
                              if checkCoordinates  placedShips coords len then
                                  return coords
                              else
                                  getShip placedShips len


getShips :: Int -> [Ship] -> IO [Ship]
getShips shipSize placedShips = if shipSize <= maxShipSize then
                                      do
                                        ship <- getShip placedShips shipSize
                                        shipList <- getShips (shipSize + 1) (ship : placedShips)
                                        return (ship : shipList)
                                  else
                                      return []

checkCoordinates :: [Ship] -> Ship -> Int -> Bool
checkCoordinates  placedShips ship shipLength
    | length ship /= shipLength = False 
    | or [coord1 == coord2 | ship2 <- placedShips, coord1 <- ship, coord2 <- ship2] = False 
    | not (and [checkValidCoord coord | coord <- ship]) = False 
    | and (map (==0) [abs ((fst coord1) - (fst coord2)) | coord1 <- ship, coord2 <- ship]) 
        = (sum [abs ((snd coord1) - (snd coord2)) | coord1 <- ship, coord2 <- ship]) * 3 == (shipLength-1) * (shipLength^2 + shipLength)
    | and (map (==0) [abs ((snd coord1) - (snd coord2)) | coord1 <- ship, coord2 <- ship]) 
        = (sum [abs ((fst coord1) - (fst coord2)) | coord1 <- ship, coord2 <- ship]) * 3 == (shipLength-1) * (shipLength^2 + shipLength)
    | otherwise = False 
