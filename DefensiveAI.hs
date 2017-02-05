module DefensiveAI (
    defAI
    ) where

import ApocTools
import System.IO
    
--Priority for this AI is to optimize defensive plays
--Will trade weaker piece for a chance to take a knight (when pawns are above 2)
--Will prioritize saving pawns when 2 or under
--Never upgrades pawn to knight when pawns are at 1
--Have to analyze the board
--Importance on opponents positions

--Needs to take the board as an arg generate a move
defAI :: Chooser
defAI b Normal        c = return (Just [(0,0),(0,0)])
defAI b PawnPlacement c = return (Just [(0,0)])


--normalDecision :: GameState -> Player -> [(Int, Int)]
--normalDecision 

pawnCount :: Player -> [[Cell]] -> Int
pawnCount Black board = foldl (\acc x -> acc + (pawnCountB x)) 0 board
pawnCount White board = foldl (\acc x -> acc + (pawnCountW x)) 0 board
                    
pawnCountB :: [Cell] -> Int
pawnCountB row = foldl (\acc x -> if x == BP
                                  then 1 + acc
                                  else 0 + acc) 0 row

pawnCountW :: [Cell] -> Int
pawnCountW row = foldl (\acc x -> if x == WP
                                  then acc + 1
                                  else acc + 0) 0 row
                                  

knightCount :: Player -> [[Cell]] -> Int
knightCount White board = foldl (\acc x -> acc + (knightCountW x)) 0 board
knightCount Black board = foldl (\acc x -> acc + (knightCountB x)) 0 board 
                      
knightCountB :: [Cell] -> Int
knightCountB row = foldl (\acc x -> if x == BK
                                    then 1 + acc
                                    else 0 + acc) 0 row
                                    
knightCountW :: [Cell] -> Int
knightCountW row = foldl (\acc x -> if x == WK
                                    then 1 + acc
                                    else 0 + acc) 0 row

--safeMove :: Player -> [[Cell]] -> [(Int, Int)]
--safeMove White board = foldl (\acc x -> 
--safeMove Black board = foldl (\acc x -> 

--safeMoveW :: Player -> [Cell] -> [(Int, Int)]
--safeMoveW c row = foldl (\acc x -> 

--moveRange :: [[Cell]] -> Cell -> (Int, Int) -> [(Int, Int)]
--moveRange board BK (x, y) = moveRangeBK
--moveRange board WK (x, y) = moveRangeWK
--moveRange board WP (x, y) = moveRangeWP
--moveRange board BP (x, y) = moveRangeBP
--moveRange board E  (x, y) = NOTHING

moveRangeK :: Player -> [[Cell]] -> (Int, Int) -> [(Int, Int)]
moveRangeK color board (a, b) = 
   delInvalMovesK 
   (board) 
   (color) 
   ([(a + x, b + y) | x <- [(-2), (-1), 1, 2], y <- [(-2), (-1), 1, 2], ((abs x) + (abs y) == 3), a + x >= 0, b + y >= 0, (a + x) < 5, (b + y) < 5])
                                        

moveRangeP :: [[Cell]] -> Player -> (Int, Int) -> [(Int, Int)]
moveRangeP board Black (a, b) = 
 (validSideM (board)
            (Black)
            ([(a+x, b+y) | y <- [(-1)], x <- [(-1), 1], a + x < 5, a + x > (-1), b + y < 5, b + y > (-1)])) ++
 (validFrontM (board)
             (Black)
             ([(a, b+y) | y <- [(-1)], b + y < 5, b + y > (-1)]))
moveRangeP board White (a, b) = 
 (validSideM (board)
             (White)
             ([(a+x, b+y) | y <- [1], x <- [(-1), 1], a + x < 5, a + x > (-1), b + y < 5, b + y > (-1)])) ++
 (validFrontM (board)
             (White)
             ([(a, b+y) | y <- [1], b + y < 5, b + y > (-1)]))

sameTeam :: Cell -> Player -> Bool
sameTeam WK White = True
sameTeam BK Black = True
sameTeam WP White = True
sameTeam BP Black = True
sameTeam _ _      = False

delInvalMovesK :: [[Cell]] -> Player -> [(Int, Int)] -> [(Int, Int)]
delInvalMovesK b c moveList = foldl (\acc x -> if (sameTeam(getFromBoard b x) c)
                                              then acc
                                              else x : acc) [] moveList
                                              
killable :: Cell -> Player -> Bool
killable BK White = True
killable BP White = True
killable WK Black = True
killable WP Black = True
killable _ _      = False
 
validSideM :: [[Cell]] -> Player -> [(Int, Int)] -> [(Int, Int)]
validSideM b c moveList = foldl (\acc x -> if (killable(getFromBoard b x) c)
                                           then x : acc
                                           else acc) [] moveList

validFrontM :: [[Cell]] -> Player -> [(Int, Int)] -> [(Int, Int)]
validFrontM b c moveList = foldl (\acc x -> if (getFromBoard b x) == E
                                            then x : acc
                                            else acc) [] moveList
