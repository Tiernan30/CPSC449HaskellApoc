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

human    :: Chooser
human b Normal        c = do
                          let x = (show c)
                          putStr ("Enter the move coordinates for player " ++ x)
                          let status = determineStatus Normal c
                          putStrLn ( " in the form 'srcX srcY destX destY' [0 >= n >= 4, or just enter return for a 'pass' " ++ status)
                          userIn <- getLine
                          if (validInput userIn 4 || userIn == [])
                          then return (returnIOMove (buildCoords Normal (buildInput userIn [])))
                          else reprompt b Normal c
human b PawnPlacement c = do
                          let x = (show c)
                          putStr ("Enter the move coordinates for player " ++ x)
                          let status = determineStatus PawnPlacement c
                          putStrLn ( " in the form 'destX destY' : [0 >= n >= 4] " ++ status)
                          userIn <- getLine
                          if (validInput userIn 2 || userIn == [])
                          then return (returnIOMove (buildCoords PawnPlacement (buildInput userIn [])))
                          else reprompt b PawnPlacement c

reprompt :: GameState -> PlayType -> Player -> IO (Maybe [(Int,Int)])
reprompt b Normal c        = do
                             let x = (show c)
                             putStr ("Enter the move coorindates for player " ++ x)
                             let status = determineStatus Normal c
                             putStrLn ( " in the form 'srcX srcY destX destY' [0 >= n >= 4, or just enter return for a 'pass' " ++ status)
                             userIn <- getLine
                             if (validInput userIn 4 || userIn == [])
                             then return (returnIOMove (buildCoords Normal (buildInput userIn [])))
                             else reprompt b Normal c
reprompt b PawnPlacement c = do
                             let x = (show c)
                             putStr ("Enter the move coordinates for player " ++ x)
                             let status = determineStatus PawnPlacement c
                             putStrLn ( " in the form 'destX destY' : [0 >= n >= 4] " ++ status)
                             userIn <- getLine
                             if (validInput userIn 2 || userIn == [])
                             then return (returnIOMove (buildCoords PawnPlacement (buildInput userIn [])))
                             else reprompt b PawnPlacement c


returnIOMove :: [(Int,Int)] -> (Maybe [(Int,Int)])
returnIOMove [] = Nothing
returnIOMove c  = Just c


validInput :: [Char] -> Int -> Bool
validInput list 0 = True
validInput [] num = False
validInput (x:xs) num  = if x `elem` ['0'..'4']
                         then isStillNum xs (num-1)
                         else if x == ' '
                              then validInput xs num
                              else False

isStillNum :: [Char] -> Int -> Bool
isStillNum [] 0       = True
isStillNum (x:xs) 0   = if x `elem` ['0'..'9']
                        then False
                        else True
isStillNum [] num     = False
isStillNum (x:xs) num = if x `elem` ['0'..'9']
                        then False
                        else if x == ' ' 
                             then validInput xs num
                             else False
   
determineStatus :: PlayType -> Player -> String
determineStatus Normal Black        = "B1:"
determineStatus Normal White        = "W1:"
determineStatus PawnPlacement Black = "B2:"
determineStatus PawnPlacement White = "W2:"

buildInput :: [Char] -> [Int] -> [Int]
buildInput [] list     = list
buildInput (x:xs) list = if x `elem` ['0'..'4']
                          then buildInput xs (list ++ [(read [x] :: Int)])
                          else buildInput xs list

buildCoords :: PlayType -> [Int] -> [(Int,Int)]
buildCoords play []            = []
buildCoords Normal list        = (list!!0, list!!1) : (list!!2, list!!3) : []
buildCoords PawnPlacement list = (list!!0, list!!1) : []
