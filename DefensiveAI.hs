module DefensiveAI (
defAI
) where

import AIFunction
import ApocTools

   
defAI :: Chooser
defAI b Normal        c = return (returnIOMove (decideMove (theBoard b) c))
defAI b PawnPlacement c = return (returnIOMove (placePawn  (theBoard b) c))

returnIOMove :: [(Int,Int)] -> (Maybe [(Int,Int)])
returnIOMove [] = Nothing
returnIOMove c  = Just c
