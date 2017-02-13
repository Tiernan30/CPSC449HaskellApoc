module ApocFunction(
  decideMove, 
  safetyOfPawn, findEndangeredLP, selectMoveP,
  normalMove, pawnCaptK,
  moveToEmpty, knightCaptPiece,
  removeMoveless, selNMovePawnID, movePawnID, selNMoveKnightID, moveKnightID, moveKnightR,
  captureMoveExist, pickKillingMove, findKnightKill, findPawnKill, knightKillable,
  --Functions that generate movements of pieces
  knightMove, bPawnSideMove, wPawnSideMove, bPawnFrontMove, wPawnFrontMove,
  
  pawnCount, knightCount,
  
  getPawnCoords, getKnightCoords,
  piecesInDanger, pieceCapturedBy,
  findKnightInDanger, isEnemyKnight, underThreatP, isEnemyPawn,
  makeCoorBoard, makeCoorBoardR,
  trackMoveableP, trackPieces, trackEmpty,
  moveRangeK, moveRangeP,
  --Pawn placement decision loops
  placePawn, getSafeSpot, removeEnds, selRandomly,
  getCordForEntity, getCord, listPick, entities, catches, whichEntity, listOfOtherMoves, dup, bestStrategies, attackByType, 
  listMoves, stillAlive, kCords, listOfCatch, caught, pCords, pawnDef, randomPick, fromJust, enforceBounds, Entity(Knight, Pawn)
) where

import ApocTools
import Data.Maybe (fromJust)
import System.IO.Unsafe
import System.Random


data Entity = Knight | Pawn deriving (Eq, Show, Read)
--Main decision tree for Normal moves

decideMove :: [[Cell]] -> Player -> [(Int, Int)]
decideMove b c 
    | (pawnCount c b) == 1 = safetyOfPawn b c (piecesInDanger b c)
    | otherwise            = normalDMove b c (piecesInDanger b c)

--Safety of Pawn    
safetyOfPawn :: [[Cell]] -> Player -> [(Cell, (Int, Int))] -> [(Int, Int)]
safetyOfPawn b c (x:xs) = if (fst x == WP || fst x == BP)
					 then findEndangeredLP (x:xs) c : (selectMoveP (moveRangeP b c (findEndangeredLP (x:xs) c)) b) :[]
					 else safetyOfPawn b c xs 
safetyOfPawn b c []     = normalDMove b c (piecesInDanger b c)

--Locate Last endangered pawn if it can move
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
    

--Normal defensive move
normalDMove :: [[Cell]] -> Player -> [(Cell, (Int, Int))] -> [(Int, Int)]
normalDMove b c []   = normalMove b c
normalDMove b c list = if findKnightInDanger list == []
				   then selNMovePawnID (removeMoveless list b c) b c (unsafePerformIO (randomRIO(0, ((length list)-1))))
				   else selNMoveKnightID b c list (unsafePerformIO (randomRIO(0, 1)))

normalMove :: [[Cell]] -> Player -> [(Int, Int)]
normalMove b c = pawnCaptK (getPawnCoords (trackMoveableP (makeCoorBoard b) b c)) b c
----------------knight check
----------------No moves so pass

--Last remaining move
moveToEmpty :: [(Cell, (Int, Int))] -> [[Cell]] -> Player -> Int -> [(Int, Int)]
moveToEmpty [] b c index = []
moveToEmpty bc b c index = if (fst (bc !! index) == WP || fst (bc !! index) == BP)
					  then (snd (bc !! index)) : (((moveRangeP b c (snd (bc !! index)))) !! 0) : []
					  else (snd (bc !! index)) : (((moveRangeK b c (snd (bc !! index)))) !! 0) : []

--Move Options
pawnCaptK :: [(Int, Int)] -> [[Cell]] -> Player -> [(Int, Int)]
pawnCaptK [] b c     = knightCaptPiece (getKnightCoords (trackMoveableP (makeCoorBoard b) b c)) b c
pawnCaptK (x:xs) b c = if knightKillable (moveRangeP b c x) b 
				   then x : (findKnightKill (moveRangeP b c x) b) : []
				   else pawnCaptK xs b c
				   
knightCaptPiece :: [(Int, Int)] -> [[Cell]] -> Player -> [(Int, Int)]
knightCaptPiece [] b c     = moveToEmpty (trackMoveableP (makeCoorBoard b) b c) b c (unsafePerformIO (randomRIO (0, ((length (trackMoveableP (makeCoorBoard b) b c)) - 1))))
knightCaptPiece (x:xs) b c = if captureMoveExist (moveRangeK b c x) b
					    then x : (findPawnKill (moveRangeK b c x) b) : [] 
					    else knightCaptPiece xs b c
				   
				   
removeMoveless :: [(Cell, (Int, Int))] -> [[Cell]] -> Player -> [(Cell, (Int, Int))]
removeMoveless bc b c = foldr (\x acc -> if (fst x == BP || fst x == WP)
								 then if (moveRangeP b c (snd x)) == []
									 then acc
									 else x : acc
								 else if (moveRangeK b c (snd x)) == []
									 then acc
									 else x : acc) [] bc

--Functions that move pieces that are in danger. Called primarily by normalDMove
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
moveKnightR movelist = movelist !! unsafePerformIO(randomRIO(0,((length movelist)-1)))

--Passed in with Pawns in danger.

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

--Takes in trackPieces argument
getPawnCoords :: [(Cell, (Int, Int))] -> [(Int, Int)]
getPawnCoords bc = foldl (\acc x -> if (fst x == BP || fst x == WP)
							 then snd x : acc
							 else acc) [] bc
							 
getKnightCoords :: [(Cell, (Int, Int))] -> [(Int, Int)]
getKnightCoords bc = foldl (\acc x -> if (fst x == WK || fst x == BK)
							   then snd x : acc
							   else acc) [] bc


piecesInDanger :: [[Cell]] -> Player -> [(Cell, (Int, Int))]
piecesInDanger b White = pieceInDanger b (trackMoveableP (makeCoorBoard b) b White) White
piecesInDanger b Black = pieceInDanger b (trackMoveableP (makeCoorBoard b) b Black) Black

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

makeCoorBoard :: [[Cell]] -> [(Cell, (Int, Int))]
makeCoorBoard b = foldr (\x acc -> (makeCoorBoardR x (4 - (quot (length acc) 5))) ++ acc) [] b 

makeCoorBoardR :: [Cell] -> Int -> [(Cell, (Int, Int))]
makeCoorBoardR row rowNum = foldr (\x acc -> (x , (4 - length acc, rowNum)) : acc ) [] row

trackMoveableP :: [(Cell, (Int, Int))] -> [[Cell]] -> Player -> [(Cell, (Int, Int))]
trackMoveableP bc b c = removeMoveless (trackPieces bc c) b c

trackPieces :: [(Cell, (Int, Int))] -> Player -> [(Cell, (Int, Int))]
trackPieces row c = foldr (\(piece, (x,y)) acc -> if sameTeam piece c 
									   then (piece, (x,y)) : acc
									   else acc) [] row

trackEmpty :: [(Cell, (Int, Int))] -> [(Int, Int)]
trackEmpty bc = foldr (\x acc -> if (fst x) == E
						   then (snd x) : acc
						   else acc) [] bc


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


getCord :: Board -> Player -> [(Int, Int)]
getCord b p = (getCordForEntity b p Knight) ++ (getCordForEntity b p Pawn)

getCordForEntity :: Board -> Player -> Entity -> [(Int, Int)]
getCordForEntity b p t | (p == White && t == Knight) = entities b WK 0 0 0 0 4 4
				  | (p == White && t == Pawn)   = entities b WP 0 0 0 0 4 4
				  | (p == Black && t == Knight) = entities b BK 0 0 0 0 4 4
				  | (p == Black && t == Pawn)   = entities b BP 0 0 0 0 4 4

-- Get the cordinates of certain cell
entities :: Board -> Cell -> Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
entities b p x y minx miny maxx maxy | (x == maxx && y == maxy) = if (getFromBoard b (maxx, maxy)) == p then (x, y) : [] else []
							| (x == maxx) = if (getFromBoard b (maxx, y)) == p then (maxx, y) : entities b p minx (y + 1) minx miny maxx maxy else entities b p minx (y + 1) minx miny maxx maxy
							| otherwise = if (getFromBoard b (x, y)) == p then (x, y) : entities b p (x + 1) y minx miny maxx maxy else entities b p (x + 1) y minx miny maxx maxy


listPick :: [a] -> Maybe a
listPick [] = Nothing
listPick (x:[]) = Just x
listPick (x:xs) | rand <= (0.7 :: Float) = Just x
						  | otherwise = listPick xs
						  where rand = unsafePerformIO (randomIO :: IO Float)

-- List of moves to catch opponenet
bestStrategies :: Board -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
bestStrategies b [] = []
bestStrategies b (p:ps) | (catch == Nothing)         = (bestStrategies b ps)
				    | (fromJust catch == Knight) = [(p, fromJust effectiveAttack)] ++ bestStrategies b ps
				    | (fromJust catch == Pawn)   = bestStrategies b ps ++ [(p, fromJust effectiveAttack)]
				    where killsList = catches b p
						effectiveAttack = listPick (attackByType b killsList)
						catch | (effectiveAttack == Nothing) = Nothing
								    | otherwise = Just $ whichEntity (pieceOf (getFromBoard b (fromJust effectiveAttack)))

attackByType :: Board -> [(Int, Int)] -> [(Int, Int)]
attackByType board [] = []
attackByType board (k:ks) | (piece == Knight) = [k] ++ (attackByType board ks)
				    | (piece == Pawn)   = (attackByType board ks) ++ [k]
				    where piece = whichEntity (pieceOf (getFromBoard board k))


listOfOtherMoves :: Board -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
listOfOtherMoves b [] = []
listOfOtherMoves b (p:ps) | (best == Nothing) = (listOfOtherMoves b ps)
					  | otherwise = (p, fromJust best) : (listOfOtherMoves b ps)
					  where movesList = listMoves b p
						   best = listPick (dup b movesList)
						   
dup :: Board -> [(Int, Int)] -> [(Int, Int)]
dup b ps = ps

whichEntity :: Piece -> Entity
whichEntity BlackKnight = Knight
whichEntity BlackPawn   = Pawn
whichEntity WhiteKnight = Knight
whichEntity WhitePawn   = Pawn

randomPick :: [a] -> Maybe a
randomPick [] = Nothing
randomPick xs = Just (xs !! index)
			   where index = floor ((unsafePerformIO (randomIO :: IO Float)) * (fromIntegral (length xs)))

pCords :: Player -> Bool -> [(Int, Int)]
pCords Black True  = [(1, -1), (-1, -1)]
pCords Black False = [(0, -1)]
pCords White True  = [(1, 1), (-1, 1)]
pCords White False = [(0, 1)]

catches :: Board -> (Int, Int) -> [(Int, Int)]
catches b pos  | (piece == Knight) = listOfCatch b kStep pos (length kStep)
			 | (piece == Pawn)   = listOfCatch b pStep pos (length pStep)
			 | otherwise = []
			 where piece = whichEntity (pieceOf (getFromBoard b pos))
				  player = playerOf (pieceOf (getFromBoard b pos))
				  kStep = kCords
				  pStep = pCords player True

listOfCatch :: Board -> [(Int, Int)] -> (Int, Int) -> Int -> [(Int, Int)]
listOfCatch b m p 0 = []
listOfCatch b (m:ms) pos index | caught b pos to = to : (listOfCatch b ms pos (index - 1))
					| otherwise = listOfCatch b ms pos (index - 1)
					where to = (fst pos + fst m, snd pos + snd m)

-- Check to see if move equates to kill
caught :: Board -> (Int, Int) -> (Int, Int) -> Bool
caught b from to = (enforceBounds from &&
			    enforceBounds to &&
			    getFromBoard b to /= E &&
			    (playerOf (pieceOf (getFromBoard b from))) /= (playerOf (pieceOf (getFromBoard b to))))

listMoves :: Board -> (Int, Int) -> [(Int, Int)]
listMoves b pos | (piece == Knight) = freeMoves b kStep pos (length kStep)
			   | (piece == Pawn)   = freeMoves b pStep pos (length pStep)
			   where piece = whichEntity (pieceOf (getFromBoard b pos))
				    player = playerOf (pieceOf (getFromBoard b pos))
				    kStep = kCords
				    pStep = pCords player False

freeMoves :: Board -> [(Int, Int)] -> (Int, Int) -> Int -> [(Int, Int)]
freeMoves b m p 0 = []
freeMoves b (m:ms) pos index | stillAlive b pos to = to : (freeMoves b ms pos (index - 1))
					   | otherwise = freeMoves b ms pos (index - 1)
					   where to = (fst pos + fst m, snd pos + snd m)

pawnDef :: Board -> Cell -> [(Int, Int)]
pawnDef b p = entities b p 0 0 0 2 4 4

stillAlive :: Board -> (Int, Int) -> (Int, Int) -> Bool
stillAlive b from to | (enforceBounds from && enforceBounds to && getFromBoard b to == E) = True
				| otherwise = False
				
-- Enforce boundries
enforceBounds      :: (Int, Int) -> Bool
enforceBounds move | a>4 = False
			 | b>4 = False
			 | a<0 = False
			 | b<0 = False
			 | otherwise = True
			 where a = fst move
				  b = snd move                   

-- Valid knight moves
kCords :: [(Int, Int)]
kCords = [(1, 2), (1, -2), (-1, 2), (-1, -2), (2, 1), (2, -1), (-2, 1), (-2, -1)]
  
validSideM :: [[Cell]] -> Player -> [(Int, Int)] -> [(Int, Int)]
validSideM b c moveList = foldl (\acc x -> if (killable(getFromBoard b x) c)
								   then x : acc
								   else acc) [] moveList

validFrontM :: [[Cell]] -> Player -> [(Int, Int)] -> [(Int, Int)]
validFrontM b c moveList = foldl (\acc x -> if (getFromBoard b x) == E
								    then x : acc
								    else acc) [] moveList
								    
--Pawn Placement decision tree
placePawn :: [[Cell]] -> Player -> [(Int,Int)]
placePawn b c = getSafeSpot b c (removeEnds (trackEmpty (makeCoorBoard b)) c)

getSafeSpot :: [[Cell]] -> Player -> [(Int, Int)] -> [(Int, Int)]
getSafeSpot b c []         = selRandomly (removeEnds (trackEmpty (makeCoorBoard b)) c)
getSafeSpot b White (x:xs) = if (captureMoveExist (moveRangeK b Black x) b || captureMoveExist (moveRangeP b Black x) b)
						then getSafeSpot b White xs
						else x: []
getSafeSpot b Black (x:xs) = if (captureMoveExist (moveRangeK b White x) b || captureMoveExist (moveRangeP b White x) b)
						then getSafeSpot b Black xs
						else x: []
						
selRandomly :: [(Int, Int)] -> [(Int, Int)]
selRandomly list =  list !! (unsafePerformIO(randomRIO(0, ((length list)-1) ))):[]

removeEnds :: [(Int, Int)] -> Player -> [(Int, Int)]
removeEnds coords White = foldr (\x acc -> if (snd x == 0)
								   then acc
								   else x : acc) [] coords
removeEnds coords Black = foldr (\x acc -> if (snd x == 0)
								   then acc
								   else x : acc) [] coords
