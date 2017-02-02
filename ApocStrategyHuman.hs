{- |
Module      : ApocStrategyHuman
Description : Template for a game-playing strategy definition.
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

This is merely a skeleton to get you started on creating a strategy for playing the
Apocalypse game.  It has VERY little functionality.
-}

module ApocStrategyHuman (
   human
   ) where

import ApocTools
import System.IO

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
getInput :: IO String
getInput = getLine

buildInput :: [Int] -> [(Int, Int)]
buildInput list 
   | (length list) == 4 = (list!!0, list!!1):(list!!2, list!!3):[]
   | (length list) == 2 = (list!!0, list !!1):[]
   | otherwise = []

convertInput :: [Char] -> [Int]
convertInput [] = []
convertInput (x:[]) = if x `elem` ['0'..'9']
                      then (read [x] :: Int):[]
                      else []
convertInput (x:xs) = if x `elem` ['0'..'9']
                      then (read [x] :: Int):convertInput xs
                      else convertInput xs


--Code Counts the number of valid inputs in a line
countInputs :: [Char] -> Int
countInputs [] = 0
countInputs (x:[]) = if x `elem` ['0'..'9']
                     then 1
                     else 0
countInputs (x:y:ys) = if x `elem` ['0'..'9'] && not (y `elem` ['0'..'9'])
                       then 1 + countInputs (y:ys)
                       else 0 + countInputs (y:ys)
 

human    :: Chooser
human b Normal        c = do
        userIn <- getLine
        return (Just (buildInput(convertInput(userIn))))
human b PawnPlacement c = return (Just [(2,2)])
