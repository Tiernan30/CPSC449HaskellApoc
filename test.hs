import System.IO
import System.Environment

main :: IO ()
main = do
   input <- getLine
   --validInput input
   newinput <- return (buildInput (convertInput input))
   putStrLn "got past"

buildInput :: [Int] -> [(Int, Int)]
buildInput list 
   | (listSize) == 4 = (list!!0, list!!1):(list!!2, list!!3):[]
   | (listSize) == 2 = (list!!0, list !!1):[]
   | otherwise = []
   where listSize = (length list)

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

validInput :: [Char] -> Bool
validInput list
   | inputNum == 0 = True
   | inputNum == 2 = True
   | inputNum == 4 = True
   | otherwise = False
   where inputNum = (countInputs list)
