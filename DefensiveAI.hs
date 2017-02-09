module DefensiveAI (
    defAI
    ) where

import ApocTools
import System.IO.Unsafe
import System.Random
    
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

--DECISION TREE FOR AI
--------------   Board     Player   Pawns  Knights   Returns Coordinate
--decideMoveN :: [[Cell]] -> Player -> Int -> Int -> [(Int, Int)]
--decideMoveN b c 1 _ = if safeOfPawn
--decideMoveN b c _ _ = --NORMAL DEFENSIVE PLAY

normalDMove :: [[Cell]] -> Player -> [(Cell, (Int, Int))] -> [(Int, Int)]
normalDMove b c []   = normalMove b c
normalDMove b c list = if findKnightInDanger list == []
                       then selNMovePawnID list b c (unsafePerformIO (randomRIO(0, ((length list)-1))))
                       else selNMoveKnightID b c list (unsafePerformIO (randomRIO(0, 1)))
                       
normalMove :: [[Cell]] -> Player -> [(Int, Int)]
normalMove b c = --Gotta Find normal 
            

selNMovePawnID :: [(Cell, (Int, Int))] -> [[Cell]] -> Player -> Int -> [(Int, Int)]
selNMovePawnID coorB b c pIndex
      | length coorB == 1 = snd (coorB !! 0) : movePawnID b (moveRangeP b c (snd (coorB !! 0))) : []
      | otherwise         = snd (coorB !! pIndex) : movePawnID b (moveRangeP b c (snd (coorB !! pIndex))) : []

movePawnID :: [[Cell]] -> [(Int, Int)] -> (Int, Int)
movePawnID b list = if captureMoveExist list b
                    then if knightKillable list b
                         then findKnightKill list b
                         else findPawnKill list b 
                     else list !! 0
            
-- Requires the board, player, listing of pieces in danger, randomly selected int between 0-1 and returns [(sx,sy),(dx,dy)]            
selNMoveKnightID :: [[Cell]] -> Player -> [(Cell, (Int, Int))] -> Int -> [(Int, Int)]
selNMoveKnightID b c coorB kIndex 
     | length coorB == 1 = snd (coorB !! 0) : moveKnightID b (moveRangeK b c (snd (coorB !! 0))) : [] 
     | otherwise         = snd (coorB !! kIndex) : moveKnightID b (moveRangeK b c (snd (coorB !! kIndex))) : []

moveKnightID :: [[Cell]] -> [(Int, Int)] -> (Int, Int)
moveKnightID b list = if captureMoveExistR list b (unsafePerformIO (randomRIO (0,10)))
                      then if knightKillable list b
                           then findKnightKill list b
                           else findPawnKill list b
                      else moveKnightR list

moveKnightR :: [(Int, Int)] -> (Int, Int)
moveKnightR movelist = movelist !! unsafePerformIO(randomRIO(0,(length movelist)))


--selectPawnID :: [(Cell, (Int, Int))] -> (Int, Int)
--movePawnID :: [[Cell]] -> Player -> (Int, Int)

--Passed in with Pawns in danger.
safetyOfPawn :: [[Cell]] -> Player -> [(Cell, (Int, Int))] -> [(Int, Int)]
safetyOfPawn b c (x:xs) = if (fst x == WP || fst x == BP)
                          then findEndangeredLP (x:xs) c : (selectMoveP (moveRangeP b c (findEndangeredLP (x:xs) c)) b) :[]
                          else safetyOfPawn b c xs 
safetyOfPawn b c []     = normalDMove b c (piecesInDanger b c)



findEndangeredLP :: [(Cell, (Int, Int))] -> Player -> (Int, Int)
findEndangeredLP (x:xs) White = if fst x == WP
                                then snd x
                                else (findEndangeredLP xs White)
findEndangeredLP (x:xs) Black = if fst x == BP
                                then snd x
                                else (findEndangeredLP xs Black)
                                
selectMoveP :: [(Int, Int)] -> [[Cell]] -> (Int, Int)
selectMoveP list board 
    | length list == 1 = list !! 0
    | otherwise = pickKillingMove list board

captureMoveExist :: [(Int, Int)] -> [[Cell]] -> Bool
captureMoveExist [] b     = False
captureMoveExist (x:xs) b = if (getFromBoard b x) == E
                            then captureMoveExist xs b
                            else True
                            
captureMoveExistR :: [(Int, Int)] -> [[Cell]] -> Int -> Bool
captureMoveExistR [] b int     = False
captureMoveExistR _ _ 0        = False
captureMoveExistR (x:xs) b int = if (getFromBoard b x) == E
                                 then captureMoveExist xs b
                                 else True
                        

pickKillingMove :: [(Int, Int)] -> [[Cell]] -> (Int, Int)
pickKillingMove list board 
    | (knightKillable list board) =  findKnightKill list board
    | otherwise                   =  findPawnKill list board
                           
findKnightKill :: [(Int, Int)] -> [[Cell]] -> (Int, Int)
findKnightKill (x:xs) b = if (getFromBoard b x == WK || getFromBoard b x == BK)
                          then x
                          else findKnightKill xs b

findPawnKill :: [(Int, Int)] -> [[Cell]] -> (Int, Int)
findPawnKill (x:xs) b = if (getFromBoard b x == WP || getFromBoard b x ==BP)
                        then x
                        else findPawnKill xs b

knightKillable :: [(Int, Int)] -> [[Cell]] -> Bool
knightKillable list b = foldl (\acc x -> if (getFromBoard b x == WK || getFromBoard b x == BK)
                                         then True
                                         else acc) False list
                                         
--getEnemyKnights :: [[Cell]] -> [(Cell, (Int, Int))] -> Player -> [(Int, Int)]
--getEnemyKnights board enemyPieces White = 
--getEnemyKnights board enemyPieces Black = 
--analysis :: [[Cell]] -> Player -> [(Int, Int)]
--analysis b c = if (piecesInDanger b c) == []
--               then captOpps b c
--               else safetyOfPawn b c (piecesInDanger b c)


--TEST BOARDS
d               :: GameState
d                = GameState Init 0 Init 0
                  [ [WK, WP, WP, WP, WK],
                    [WP, E , E , E , WP],
                    [E , E , E , E , E ],
                    [E , E , E , E , BP ],
                    [E , E , BK, E , E ]]


c               :: GameState
c               = GameState Init 0 Init 0
                  [ [WK, WP, WP, WP, WP],
                    [WP, E , BP, E , WP],
                    [E , E , E , E , E ],
                    [E , E , E , E , E ],
                    [E , E , E , E , BP]]

--THESE FUNCTIONS IMPORTANT
--List Comprehension methods that generate moves that can be made within a 5 x 5 grid for the respective pieces
knightMove = (\a b -> [(a + x, b + y) | x <- [(-2), (-1), 1, 2], y <- [(-2), (-1), 1, 2], ((abs x) + (abs y) == 3), a + x >= 0, b + y >= 0, (a + x) < 5, (b + y) < 5])
bPawnSideMove = (\a b -> [(a+x, b+y) | y <- [(-1)], x <- [(-1), 1], a + x < 5, a + x > (-1), b + y < 5, b + y > (-1)])
wPawnSideMove = (\a b -> [(a+x, b+y) | y <- [1], x <- [(-1), 1], a + x < 5, a + x > (-1), b + y < 5, b + y > (-1)])
bPawnFrontMove = (\a b -> [(a, b+y) | y <- [(-1)], b + y < 5, b + y > (-1)])
wPawnFrontMove = (\a b -> [(a, b+y) | y <- [1], b + y < 5, b + y > (-1)])

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


piecesInDanger :: [[Cell]] -> Player -> [(Cell, (Int, Int))]
piecesInDanger b White = pieceInDanger b (trackPieces (makeCoorBoard b) White) White
piecesInDanger b Black = pieceInDanger b (trackPieces (makeCoorBoard b) Black) Black

pieceInDanger :: [[Cell]] -> [(Cell, (Int, Int))] ->  Player -> [(Cell, (Int, Int))]
pieceInDanger b bc Black = foldl (\acc x -> if (pieceCapturedBy (moveRangeK b Black (snd x)) WK b Black) || (pieceCapturedBy (moveRangeP b Black (snd x)) WP b Black)
                                            then x : acc
                                            else acc) [] bc
pieceInDanger b bc White = foldl (\acc x -> if (pieceCapturedBy (moveRangeK b White (snd x)) BK b White) || (pieceCapturedBy (moveRangeP b White (snd x)) BP b White)
                                            then x : acc
                                            else acc) [] bc

pieceCapturedBy :: [(Int, Int)] -> Cell -> [[Cell]] -> Player -> Bool
pieceCapturedBy list piece b c = foldl (\acc x -> if (getFromBoard b x) == piece
                                                      then True
                                                      else acc) False list
                                                         
findKnightInDanger :: [(Cell, (Int, Int))] -> [(Cell, (Int, Int))]
findKnightInDanger bc = foldr (\x acc -> if (fst x == WK || fst x == BK)
                                         then x : acc
                                         else acc) [] bc

isEnemyKnight :: Cell -> Player -> Bool
isEnemyKnight WK Black = True
isEnemyKnight BK White = True
isEnemyKnight _ _      = False

underThreatP :: [(Int, Int)] -> [[Cell]] -> Player -> Bool
underThreatP list b White = foldl (\acc x -> isEnemyPawn (getFromBoard b x) White) False list
underThreatP list b Black = foldl (\acc x -> isEnemyPawn (getFromBoard b x) Black) False list

isEnemyPawn :: Cell -> Player -> Bool
isEnemyPawn WP Black = True
isEnemyPawn BP White = True
isEnemyPawn _ _      = False

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
