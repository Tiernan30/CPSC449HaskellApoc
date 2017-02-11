import System.Environment
import System.IO.Unsafe
import ApocTools
import DefensiveAI
import ApocStrategyHuman

main :: IO ()
main = interactiveMode (unsafePerformIO getArgs)

interactiveMode :: [String] -> IO()
interactiveMode args 
    | args == []         = do
                           putStr "Launching Interactive Mode"
    | (length args) == 2 = do 
                           let wStrat = (args !! 0)
                           let bStrat = (args !! 1)
                           
                           (parseStrategy wStrat) initBoard Normal Black
                           
                           putStr "Strategies Loaded"
    | otherwise          = putStr "Input invalid"
                      
parseStrategy :: String -> Chooser
parseStrategy "human"     = human
parseStrategy "Human"     = human
parseStrategy "Defensive" = defAI


