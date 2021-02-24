import Data.List
import Coordinate
import Ship
import Board


players = ("Player1" , "Player2")

data GameSettings = GameSettings 
    { playerOneBoard :: Board
    , playerTwoBoard :: Board
    , playerOneStartShips:: [Ship]
    , playerTwoStartShips:: [Ship]
    }

startGame :: IO ()
startGame = do
        print(fst players ++ "enter ships is format (x,y);(x+1,y) and so on")
        shipsPlayer1 <- getShips Ship.minShipSize []
        let player_one_board = startBoard
        print(snd players ++ "enter ships is format (x,y);(x+1,y) or so on")
        shipsPlayer2 <- getShips Ship.minShipSize []
        let player_two_board = startBoard

        let settings = GameSettings player_one_board player_two_board shipsPlayer1 shipsPlayer2

        play players [ playerOneBoard settings,playerTwoBoard settings] [playerOneStartShips settings , playerTwoStartShips settings]
        

play :: (String, String) -> [Board] -> [[Ship]] -> IO ()
play names fields ships = do
                            print ("turn " ++fst names )
                            printBoard (snd names) (last fields) (last ships)
                            (newField, newShipList) <- shipsFire (last fields, last ships) (head ships)
                            if length newShipList == 0 then
                                do
                                  print (fst names ++ " won!")
                                  printBoard (snd names) newField newShipList
                                  printBoard (fst names) (head fields) (head ships)
                            else
                                  play (snd names, fst names) [newField, head fields] [newShipList, head ships]


markShot :: Board -> Int -> Int -> Board
markShot field x y = replaceNth x  (replaceNth y True (field !! x)) field 

destroyShip :: [Ship] -> [Ship]
destroyShip [] = []
destroyShip (x:xs) | null x    = destroyShip xs
                            | otherwise = x : destroyShip xs

checkShipDestroyed :: Board -> Ship -> Coordinate -> (Ship, Bool)
checkShipDestroyed field ship coordinate = if or [coordinate == coord | coord <- ship] == False then do (ship, False)   
                                           else do
                                               if and [ (field !! (snd coord - 1)) !! (fst coord - 1) == True | coord <- ship, coord /= coordinate] == False 
                                                then (ship, True) 
                                               else  ([], True)   


shoot :: (Board, [Ship]) -> Coordinate -> (Board, [Ship], Bool)
shoot (enemyField, enemyShips) coordinate = (markShot enemyField (snd coordinate - 1) (fst coordinate - 1),
                                            destroyShip [fst (checkShipDestroyed enemyField ship coordinate) | ship <- enemyShips],
                                            or [snd (checkShipDestroyed enemyField ship coordinate) | ship <- enemyShips] )
                                           

-- hitOrMiss:: Board -> [Ship] -> Coordinate -> Cell
-- hitOrMiss enemyField enemyShips coordinate = if  (or [snd (checkShipDestroyed enemyField ship coordinate) | ship <- enemyShips]) 
--                                                     then Hit
--                                                     else Miss

shipsFire :: (Board, [Ship]) -> [Ship] -> IO (Board, [Ship])
shipsFire (enemyField, enemyShips) [] = return (enemyField, enemyShips)
shipsFire (enemyField, enemyShips) ourShips = do
                print ("make a shot per each ship you have")
                shot <- getLine
                let coord = stringToCoord shot
                if checkValidCoord coord then
                 do let (newEnemyField, newEnemyShips, shot) = shoot (enemyField, enemyShips) coord
                    if (shot)  then print ("hit!!!!!")
                               else print ("Miss(")
                    if length newEnemyShips < length enemyShips then do
                        print "ship destroyed"
                        shipsFire (newEnemyField, newEnemyShips) (tail ourShips)
                    else
                        shipsFire (newEnemyField, newEnemyShips) (tail ourShips)
                else
                    shipsFire (enemyField, enemyShips) ourShips

