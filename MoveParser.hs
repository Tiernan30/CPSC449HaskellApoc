module MoveParser (
parseMoves,
normalMParse, whiteNMParse, blackNMParse,
placePawnMParse
) where

import ApocTools
import ApocFunction
import Data.Maybe
import Data.List

parseMoves :: PlayType -> Maybe [(Int, Int)] -> [[Cell]] -> Player -> Played
parseMoves Normal move b c        = normalMParse move b c
parseMoves PawnPlacement move b c = placePawnMParse move b c

normalMParse :: Maybe [(Int, Int)]-> [[Cell]] -> Player -> Played
normalMParse move b c
    | move == Nothing = Passed
    | c == White      = whiteNMParse (fromJust move) b (getFromBoard b (head (fromJust move)))
    | c == Black      = blackNMParse (fromJust move) b (getFromBoard b (head (fromJust move)))

whiteNMParse :: [(Int,Int)] -> [[Cell]] -> Cell -> Played
whiteNMParse coord b piece
    | piece == WP = if (coord!!1) `elem` (moveRangeP b White (coord!!0))
                    then Played ((coord!!0),(coord!!1))
                    else Goofed ((coord!!0),(coord!!1))
    | piece == WK = if (coord!!1) `elem` (moveRangeK b Black (coord!!0))
                    then Played ((coord!!0),(coord!!1))
                    else Goofed ((coord!!0),(coord!!1))

blackNMParse :: [(Int,Int)] -> [[Cell]] -> Cell -> Played 
blackNMParse coord b piece
    | piece == BP = if (coord!!1) `elem` (moveRangeP b Black (coord!!0))
                    then Played ((coord!!0),(coord!!1))
                    else Goofed ((coord!!0),(coord!!1))
    | piece == BK = if (coord!!1) `elem` (moveRangeK b Black (coord!!0))
                    then Played ((coord!!0),(coord!!1))
                    else Goofed ((coord!!0),(coord!!1))

placePawnMParse :: Maybe [(Int, Int)] -> [[Cell]] -> Player -> Played
placePawnMParse list b c
    | list == Nothing = NullPlacedPawn
    | c == White      = if ((fromJust list)!!0) `elem` (removeEnds (trackEmpty (makeCoorBoard b)) White)
                        then PlacedPawn ((getCoordPawnE (b!!0) White),(head (fromJust list)))
                        else BadPlacedPawn ((getCoordPawnE (b!!0) White),(head (fromJust list)))
    | c == Black      = if ((fromJust list)!!0) `elem` (removeEnds (trackEmpty (makeCoorBoard b)) Black)
                        then PlacedPawn ((getCoordPawnE (b!!4) Black),(head (fromJust list)))
                        else BadPlacedPawn ((getCoordPawnE (b!!4) Black) ,(head (fromJust list)))
                        
                        
getCoordPawnE :: [Cell] -> Player -> (Int, Int)
getCoordPawnE b White = ((fromJust (WP `elemIndex` b)), 4)
getCoordPawnE b Black = ((fromJust (BP `elemIndex` b)), 0)


