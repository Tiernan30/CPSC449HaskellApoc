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

--THESE FUNCTIONS IMPORTANT
--List Comprehension methods that generate moves that can be made within a 5 x 5 grid for the respective pieces
knightMove = (\a b -> [(a + x, b + y) | x <- [(-2), (-1), 1, 2], y <- [(-2), (-1), 1, 2], ((abs x) + (abs y) == 3), a + x >= 0, b + y >= 0, (a + x) < 5, (b + y) < 5])
bPawnSideMove = (\a b -> [(a+x, b+y) | y <- [(-1)], x <- [(-1), 1], a + x < 5, a + x > (-1), b + y < 5, b + y > (-1)])
wPawnSideMove = (\a b -> [(a+x, b+y) | y <- [1], x <- [(-1), 1], a + x < 5, a + x > (-1), b + y < 5, b + y > (-1)])
bPawnFrontMove = (\a b -> [(a, b+y) | y <- [(-1)], b + y < 5, b + y > (-1)])
wPawnFrontMove = (\a b -> [(a, b+y) | y <- [1], b + y < 5, b + y > (-1)])


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

piecesInDanger :: [[Cell]] -> Player -> [(Cell, (Int, Int))]
piecesInDanger b White = (trackPieces (makeCoorBoard b) White)
piecesInDanger b Black = (trackPieces (makeCoorBoard b) Black)

--inDanger :: [[Cell]] -> Player -> (Int, Int) -> Bool
--inDanger b White pos =
--inDanger b Black pos = 

makeCoorBoard :: [[Cell]] -> [[(Cell, (Int, Int))]]
makeCoorBoard b = foldr (\x acc -> (makeCoorBoardR x (4 - length acc)) : acc) [] b 

makeCoorBoardR :: [Cell] -> Int -> [(Cell, (Int, Int))]
makeCoorBoardR row rowNum = foldr (\x acc -> (x , (4 - length acc, rowNum)) : acc ) [] row

trackPieces :: [[(Cell, (Int, Int))]] -> Player -> [(Cell, (Int, Int))]
trackPieces b c = foldr (\x acc -> (trackPiecesI x c) ++ acc) [] b

trackPiecesI :: [(Cell, (Int, Int))] -> Player -> [(Cell, (Int, Int))]
trackPiecesI row c = foldr (\(piece, (x,y)) acc -> if sameTeam piece c 
                                                         then (piece, (x,y)) : acc
                                                         else acc) [] row

moveRangeK :: [[Cell]] -> Player -> (Int, Int) -> [(Int, Int)]
moveRangeK board color (a, b) = 
   delInvalMovesK 
   (board) 
   (color) 
   (knightMove a b)
                                        
moveRangeP :: [[Cell]] -> Player -> (Int, Int) -> [(Int, Int)]
moveRangeP board Black (a, b) = 
    (validSideM (board) (Black) (bPawnSideMove a b)) ++
    (validFrontM (board) (Black) (bPawnFrontMove a b))
moveRangeP board White (a, b) = 
    (validSideM (board) (White) (wPawnSideMove a b)) ++
    (validFrontM (board) (White) (wPawnFrontMove a b))

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
