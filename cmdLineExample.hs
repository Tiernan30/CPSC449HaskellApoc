import System.Environment
import System.IO.Unsafe
import ApocTools
import DefensiveAI
import ApocStrategyHuman
import Data.Maybe

main :: IO ()
main = interactiveMode (unsafePerformIO getArgs)

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
                    move <- s1 b Normal White
                    move2 <- s2 b Normal Black
                    putStr "Hello"
