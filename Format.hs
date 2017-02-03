module Format where

formatTableNS :: Int ->  [[String]] -> String
formatTableNS cWidth list = foldr (\x acc -> (printString cWidth x ++ "\n" ++ acc)) [] list

printString :: Int -> [String] -> String
printString width list = foldr (\x acc -> x ++ (concat $ replicate (width - length x)  " ")  ++ acc) [] list

formatTableS :: [[String]] -> String
formatTableS l = formatTableNS ((getMax (twoToOne l)) + 1) l

formatTable :: (Show a) => [[a]] -> String
formatTable l = formatTableNS ((getMax (twoToOne  (turnToString l )))+1) (turnToString l) 

--Converts a 2D list to a 1D list using folds
twoToOne :: [[a]] -> [a]
twoToOne list = foldl (\acc x -> x ++ acc) [] list

--Finds the maximum length elements in a list of strings
getMax list = foldr (\x acc -> if ((length x) > acc )
                               then (length x) 
                               else acc) 0 list

--Converts a list into a list of strings using show
turnToString l = map (\x -> map show x) l
