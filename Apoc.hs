{- |
Module      : Main
Description : Template to get you started on the CPSC 449 Winter 2016 Apocalypse assignment.
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3
This module is used for CPSC 449 for the Apocalypse assignment.
Feel free to modify this file as you see fit.
-}

module Main (
      -- * Main
      main,
      -- * Utility functions
      replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman
import DefensiveAI
import MoveParser
  
---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = interactiveMode (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:
     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}

-- | The interactiveMode, used to do initial IO, handling the launch of the game whether it has 2 appropriate arguements, or needs to prompt for them.
interactiveMode :: [String] -- ^ The List of Strings arguement.
                   -> IO()  -- ^ The IO output, either a stirng or the initial board state.
interactiveMode args 
    | args == []         = do  -- $interactiveDoc 
                           putStrLn "---------------------------------Launching Interactive Mode---------------------------------"
                           putStr "Apocalypse is a chess variant game that is played on a 5x5 board, each player starts with 2 knights and 5 pawns. "
                           putStr "Players move at the same time each turn. If two knights try to take one another, then the knights both simultaneously "
                           putStr "move to where the previous knights were. If in the case where two pieces move to the same spot then the special rules apply. "
                           putStr "If two knights collide, both knights are taken. The same happens for two pawns. If a collision between a knight and a "
                           putStrLn "pawn occur then the knight trumps and the pawn is captured."
                           putStr "The following strategies are available to be loaded in for play (strategies must be entered as displayed below) : \n"
                           putStrLn "  human"
                           putStrLn "  defensive"
                           putStrLn "Please choose a strategy for black player"
                           bStrat <- getLine
                           putStrLn "Please choose a strategy for white player"
                           wStrat <- getLine
                           if (checkStrategy wStrat) && (checkStrategy bStrat)
                           then startNextTurn initBoard (parseStrategy bStrat) (parseStrategy wStrat)
                           else putStrLn "Invalid strategies selected, please select from the following: \n  human \n  defensive"                    
                           
    | (length args) == 2 = do 
                           let wStrat = (args !! 0)
                           let bStrat = (args !! 1)
                           if (checkStrategy wStrat) && (checkStrategy bStrat)
                           then startNextTurn initBoard (parseStrategy bStrat) (parseStrategy wStrat)
                           else putStrLn "Invalid strategies selected, please select from the following: \n  human \n  defensive"  
                           
                           putStr "Strategies Loaded"
    | otherwise          = putStrLn "Invalid strategies selected, please select from the following: \n  human \n  defensive"
                      
-- $interactiveDoc
-- This block is for interactiveMode. If there are no args, it's a long series of printed strings followed by a check to see if the 
-- arguements obtained from user input are valid or not.  If so, starts the next (first) turn of the game with initBoard and the chosen strategies.
-- If there are 2 arguements when it is called, it checks if they are both valid, if not it prints the invalid message, if so it starts the next(first) turn with initBoard and the chosen strategies.
-- If there are more or less than two arguements, the invalid message is printed and the program ends $interactiveDoc.

-- | Function to check if a given strategy is valid
checkStrategy :: String -- ^ The String argument
              -> Bool   -- ^ The result as a Bool
checkStrategy "human"     = True  -- ^ if the string reads "human" return True
checkStrategy "defensive" = True  -- ^ if the string reads "defensive" return True
checkStrategy _           = False -- ^ anything else return False

-- | The function to select a chooser based on the given string 
parseStrategy :: String  -- ^ The string argument
              -> Chooser -- ^ The result as a chooser
parseStrategy "human"     = human -- ^ if the string is "human" return the human chooser
parseStrategy "defensive" = defAI -- ^ if the string is "defensive" return the defAI chooser


-- | The function to call the game "loop".
startNextTurn :: GameState -- ^ The GameState arguement
              -> Chooser   -- ^ The Chooser arguement for the black player
			  -> Chooser   -- ^ The Chooser arguement for the white player
			  -> IO ()     -- ^ The next GameState + updated game as output is returned
startNextTurn currentstate blackstrategy whitestrategy = do
  
  let movetypes = getPlayTypes currentstate Black White
  
  -------------------------
  
  blackmove <- blackstrategy currentstate (movetypes !! 0) Black
  whitemove <- whitestrategy currentstate (movetypes !! 1) White

  
  let nextstate = processMoves currentstate blackmove whitemove
  
  
  -------------------------------------
  
  putStrLn (show $ nextstate)
  
--  if ((hasPromotionOrPawnPlacement nextstate Black) == 1 || (hasPromotionOrPawnPlacement nextstate White) == 1)
--    then 
        --promoting a pawn is fully automatic so it just has it's own turn logic
--        promotePawnTurn nexstate blackstrategy whitestrategy blackmove whitemove
--            else 
--                ( case checkGameOver nextstate of
--                    0 -> startNextTurn nextstate blackstrategy whitestrategy
--                    1 -> gameWon nextstate Black
--                    2 -> gameWon nextstate White
--                    3 -> gameTied nextstate )
  
 
------------- Promote Pawn Turn ----------------

--promotePawnTurn :: g -> Chooser -> Chooser-> PlayType -> PlayType -> IO ()
--promotePawnTurn currentstate blackstrategy whitestrategy blackplaytype whiteplaytype = do
--    
--    
--    let blackplayed = if ((hasPromotionOrPawnPlacement nextstate Black) == 1)
--                        then
--                            UpgradedPawn2Knight
--                                else
--                                    None
--                                    
--    let whiteplayed = if ((hasPromotionOrPawnPlacement nextstate White) == 1)
--                        then
--                            UpgradedPawn2Knight
--                                else
--                                    None
--    
--    promotePawnBoard = promotePawns theBoard currentstate
    




-----------------------------------------------
--v2vpb
--processMoves :: GameState -> Maybe [(Int,Int)] -> Maybe [(Int, Int)]-> GameState
--processMoves _ _ = initBoard

gameWon :: GameState -> Player -> IO ()
gameWon g White = putStrLn "\nWhite Wins!"
gameWon g Black = putStrLn "\nBlack Wins!"

gameTied :: GameState -> IO ()
gameTied g = putStrLn "\nGame is a tie!"

getPlayTypes :: GameState -> Player -> Player -> [PlayType]
getPlayTypes g b w
    | hasPromotionOrPawnPlacement g Black == 0 = (PawnPlacement : Normal : [])
    | hasPromotionOrPawnPlacement g White == 0 = (Normal : PawnPlacement : [])
    | otherwise =  (Normal : Normal : [])

-------------------strategy----------

getStrategy :: String -> Chooser
getStrategy _ = human
--getStrategy "defensiveAI" = defensiveAI
--getStrategy "agressiveAI" = agressiveAI

getPlayType :: Int -> PlayType
getPlayType 0 = Normal
getPlayType 1 = PawnPlacement

-------------------------------------------------------------------
-- 0? no one won, 1? White won, 2? Black won, 3? tie
checkGameOver :: GameState -> Int
checkGameOver g
    | blackPen g >= 2 && whitePen g >= 2 = 3
    | blackPen g >= 2 = 2
    | whitePen g >= 2 = 1 
    | blackPlay g == Passed && whitePlay g == Passed = 3
    | countPieceBoard (theBoard g) BP == 0 && countPieceBoard (theBoard g) WP == 0 = 3
    | countPieceBoard (theBoard g) BP == 0 = 2
    | countPieceBoard (theBoard g) WP == 0 = 1
    | otherwise = 0
    
    
-----------------------Returns whether or not there are any permissible moves left in the board
noMovesLeft :: Board -> Bool
noMovesLeft b
    | hasMoves b White == False && hasMoves b Black == False = True
    | otherwise = False
    
hasMoves :: Board -> Player -> Bool
hasMoves b White = False
hasMoves b Black = False
-----------------------Are there [certain piece] left? Returns the number of said piece -----------
countPieceBoard :: Board -> Cell -> Int
countPieceBoard [] _ = 0
countPieceBoard (r:rs) t = countPieceRow r t + countPieceBoard rs t

countPieceRow :: [Cell] -> Cell -> Int
countPieceRow [] _ = 0
countPieceRow (c:cs) t
    | c == t = 1 + countPieceRow cs t
    | otherwise = 0 + countPieceRow cs t
    
---- given a cell, whether or not it can eat something, if it can, returns the cell it can eat 
--canEatSomething
    -- 

--handles all pawn placement logic and implementation for a given player
--2? nothing, 1? promotePawn, 0? pawnPlacement
hasPromotionOrPawnPlacement ::  GameState -> Player -> Int
hasPromotionOrPawnPlacement g Black
    | hasPawnReachedEnd g Black == False = 2
    | lessThanTwoKnights g Black == True = 1
    | otherwise = 0
hasPromotionOrPawnPlacement g White
    | hasPawnReachedEnd g White == False = 2
    | lessThanTwoKnights g White == True = 1
    | otherwise = 0
    
hasPawnReachedEnd :: GameState -> Player -> Bool
hasPawnReachedEnd g White
    | countPieceRow (last (theBoard g)) WP == 0 = False
    | otherwise = True
hasPawnReachedEnd g Black
    | countPieceRow (head (theBoard g)) BP == 0 = False
    | otherwise = True
    
lessThanTwoKnights :: GameState -> Player -> Bool
lessThanTwoKnights g Black
    | countPieceBoard (theBoard g) BK < 2 = True
    | otherwise = False
lessThanTwoKnights g White
    | countPieceBoard (theBoard g) WK < 2 = True
    | otherwise = False
    
--REDO THIS FUNCTION
--promotePawns :: GameState -> Player -> [(Int, Int)]
--promotePawns g Black =  [(getPieceRow (theBoard g !! 0) BP,0),(getPieceBoard (theBoard g !! 4) WP,4)]
--promotePawns g White =  [(getPieceRow (theBoard g !! 4) WP,4),(getPieceBoard (theBoard g !! 4) WP,4)]

getPieceRow :: [Cell] -> Cell -> Int
getPieceRow [] _ = 0
getPieceRow (c:cs) t
    | c == t = 0
    | otherwise = 1 + getPieceRow cs t
    

-- | Function for pawn placement
pawnPlace :: [[Cell]]     -- ^ The list of list of Cells arguement (the game board)
          -> [(Int, Int)] -- ^ The list of tuples of ints argument (the coordinates)
		  -> [[Cell]]     -- ^ The updated board is returned as output
pawnPlace b [(x1, y1), (x2,y2)] = (replace2 (replace2 b (x2, y2) (getFromBoard b (x1,y1))
                                            (x1,y1)
                                            E))
-- | The function to handle moves when a collision occurs, and both pieces are discarded
twoMovesC :: [[Cell]]     -- ^ The board arguement
          -> [(Int, Int)] -- ^ The first set of coordinates arguement (the second tuple can be anything because it will be the same for both due to other checks.)
		  -> [(Int, Int)] -- ^ The second set of coordinates argument
		  -> [[Cell]]     -- ^ The returned, updated board
twoMovesC b [(a1, b1), _ ] [(x1,y1), _]   = (replace2 (replace2 b (a1,b1) E)
                                                                (x1,y1)
                                                                E)

-- | Function to check whether to destination moves will result in a collision.
checkCollision :: (Int, Int)  -- ^ The First coordinate arguement
               -> (Int, Int)  -- ^ The Second coordinate arguement
			   -> Bool        -- ^ The result as a bool
checkCollision (x1, y1) (x2, y2) = if x1 == x2 && y1 == y2 then True
                                           else False
                                           
-- | Function to check what to do if a collision does occur
checkClash :: [[Cell]]     -- ^ The board arguement
           -> [(Int, Int)] -- ^ The first set of coordinates argument
		   -> [(Int, Int)] -- ^ The second set of coordinates argument
		   -> [[Cell]]     -- ^ The updated board returned
-- $clashDoc.		   
checkClash b [(a1, b1), (a2,b2)] [(x1, y1), (x2, y2)] = if (checkCollision (a2, b2) (x2, y2)) == True
                                    then threeMoves b [(a1, b1), (a2,b2)] [(x1, y1), (x2, y2)]
                                    else fourMoves b [(a1, b1), (a2,b2)] [(x1, y1), (x2, y2)]
-- $clashDoc                  
-- This section takes the two tuples of coordinates (black player source move and destination move and white player source and destination)
-- if the collision check returns true, then use three moves (as 2 moves will be E, the last will be the winning piece) otherwise
-- use fourMoves as there was no collision #clashDoc.                
                                    
-- | Function to check if collision occurs, handle the collision, if both pieces are pawns or knights, it's a loss clash, so twoMoves, otherwise update the board.
threeMoves :: [[Cell]]     -- ^ The Board arguement
           -> [(Int, Int)] -- ^ The first set of coordinates argument
		   -> [(Int, Int)] -- ^ The second set of coordinates argument
		   -> [[Cell]]     -- ^ The updated board returned

-- $threeMovesDoc
threeMoves b [(a1, b1), (a2,b2)] [(x1, y1), (x2, y2)] = if( getFromBoard b (a1, b1) == WP && getFromBoard b (x1, y1) == BP 
                                   || getFromBoard b (a1, b1) == WK && getFromBoard b (x1, y1) == BK)                                                    
                                   then twoMovesC b [(a1, b1), (a2,b2)] [(x1, y1), (x2, y2)] 
                                                                  
                                   else (if (getFromBoard b (a1, b1) == WK && getFromBoard b (x1, y1) == BP)
                                         then (replace2 (replace2 (replace2 (b)
                                                                                     (x1, y1) 
                                                                                     E) 
                                                                            (a2, b2) 
                                                                            WK)
                                                                 (a1, b1) 
                                                                 E)
                                         else (  (replace2 (replace2 (replace2 (b) 
                                                                                    (x2, y2) 
                                                                                     BK)
                                                             (x1, y1) 
                                                              E) 
                                                  (a1, b1) 
                                                   E)))                                     
-- $threeMovesDoc
-- This block takes the black/white player source and destination coordinates, and checks which pieces are at the source.
-- If they are the same, calls twoMoves.  If they are different, then check to see which of them is a Knight and update the board $threeMovesDoc.

-- | The function to update the board for a normal turn when no clash occurs.
fourMoves :: [[Cell]]     -- ^ The board argument
          -> [(Int, Int)] -- ^ The first set of coordinates argument
		  -> [(Int, Int)] -- ^ The second set of coordinates argument
		  -> [[Cell]]     -- ^ The updated board returned
		  
-- $fourMovesDoc		  
fourMoves b [(a1, b1), (a2,b2)] [(x1, y1), (x2, y2)] = (replace2 (replace2 (replace2 (replace2 (b) 
                                                                                               (x2, y2) 
                                                                                               (getFromBoard b (x1,y1)))
                                                                                     (x1, y1) 
                                                                                     E) 
                                                                            (a2, b2) 
                                                                            (getFromBoard b (a1,b1)))
                                                                 (a1, b1) 
                                                                 E)    
-- $fourMovesDoc
-- This block takes the black and white source and destination coordinates, then updates the board appropriately $fourMovesDoc.
