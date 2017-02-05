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
      main, main',
      -- * Utility functions
      replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
    putStrLn "\nThe initial board:"
    print initBoard

    putStrLn $ "\nThe initial board with back human (the placeholder for human) strategy having played one move\n"
               ++ "(clearly illegal as we must play in rounds!):"
    move <- human (initBoard) Normal Black
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
    newBoard <- return (GameState (if move==Nothing
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
   
    startNextTurn newBoard



startNextTurn                       :: GameState -> IO ()
startNextTurn boardState = do
             
--Apply strategies
  blackmove <- human (boardState) Normal Black
  
  whitemove <- return (Just [(4,4), (3,3)])
  
  newBoard <- return (GameState (if blackmove==Nothing then Passed else Played (head (fromJust blackmove), head (tail (fromJust blackmove))))
                     (blackPen boardState)
                     (if whitemove==Nothing then Passed else Played (head (fromJust whitemove), head (tail (fromJust whitemove))))
                     (whitePen boardState)
                     (replace2 (replace2 (theBoard boardState)
                                                   ((fromJust blackmove) !! 1)
                                                   (getFromBoard (theBoard boardState) ((fromJust blackmove) !! 0)))
                                         ((fromJust blackmove) !! 0)
                                         E))
                     
  
  putStrLn (show $ newBoard)
  
  startNextTurn newBoard

getInput :: IO String
getInput = getLine 

--Could also GameState -> Bool, and extract data from the structure that is GameState	
--Basically the @ function assigns a name to a specific part of a bigger data type
--as below player is the name, whitePlayed is the data type inside GameState
--So, we're extracting the data from whitePlayed which is type Played and we only care about 
--the second tuple in the pair of tuples, and thus have named it s@ second tuble values
--It's unfinished at the moment as I wasn't sure of the format we as a group wanted to follow.
--Basically, options are GameState ->Bool, and extract the data, or
--GameState -> PlayType -> Player -> Played -> Bool
--I'm leaning extracting the data though, as that makes it more useful.
--isValid :: GameState -> PlayType -> Player -> Played -> Bool
isValid :: GameState -> PlayType -> Bool
isValid GameState{theBoard = nBoard, whitePlayed = wPlayer@(Played (srcW@(x,y), dstW@(xW,yW+1)))||
                  blackPlayed = bPlayer@(Played (srcB@(xB,yB), dstB(xB,yB -1))) } Normal move = if getFromBoard nBoard dstW || dstB == "_" 
                                                                                         then True
                                                                                         else False


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

