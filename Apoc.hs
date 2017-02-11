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
      main, interactiveMode,
      -- * Utility functions
      replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman
import DefensiveAI


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.

main :: IO ()
main = interactiveMode (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}

interactiveMode :: [String] -> IO()
interactiveMode args 
    | args == []         = do
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
                           then nextStage initBoard (parseStrategy bStrat) (parseStrategy wStrat)
                           else putStrLn "Invalid strategies selected, please select from the following: \n  human \n  defensive"                    
                           
    | (length args) == 2 = do 
                           let wStrat = (args !! 0)
                           let bStrat = (args !! 1)
                           if (checkStrategy wStrat) && (checkStrategy bStrat)
                           then nextStage initBoard (parseStrategy bStrat) (parseStrategy wStrat)
                           else putStrLn "Invalid strategies selected, please select from the following: \n  human \n  defensive"  
                           
                           putStr "Strategies Loaded"
    | otherwise          = putStr "Input invalid"
                      

checkStrategy :: String -> Bool
checkStrategy "human"     = True
checkStrategy "defensive" = True
checkStrategy _           = False

parseStrategy :: String -> Chooser
parseStrategy "human"     = human
parseStrategy "defensive" = defAI


nextStage :: GameState -> Chooser -> Chooser -> IO ()
nextStage b s1 s2 = do
                    
                    move <- s1 b Normal Black
                    move2 <- s2 b Normal White
                    
                    putStrLn "\nThe initial board:"
                    print initBoard
                    putStrLn "This is a description"
                    
                    putStrLn (show $ GameState (if move==Nothing
                                                then Passed
                                                else Played (head (fromJust move), head (tail (fromJust move))))
                                                (blackPen initBoard)
                                                (Passed)
                                                (whitePen initBoard)
                                                (replace2 (replace2 (theBoard initBoard)
                                                                    ((fromJust move) !! 1)
                                                                    (getFromBoard (theBoard initBoard) ((fromJust move) !! 0)))
                                                          ((fromJust move) !! 0)
                                                          E))
                                                          
                    putStr "Hello"

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

