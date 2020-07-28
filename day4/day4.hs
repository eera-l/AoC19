import Data.List.Split
import Data.List

input :: [Char]
input = "272091-815432"

task1 :: Int
task1 = do let l_range = read ((splitOn "-" input)!!0) :: Int
           let h_range = read ((splitOn "-" input)!!1) :: Int
           let nums = [n | n <- [l_range..h_range],
                           containsDouble (show n),
                           increasing (show n)]
           length nums


-- Checks if string contains doubles
containsDouble :: [Char] -> Bool
containsDouble []                        = False
containsDouble (x:xs) | (length xs) > 0  = (x == (head xs)) || (containsDouble xs)  
                      | otherwise        = False   


-- Checks if characters in string are in increasing order
increasing :: [Char] -> Bool
increasing s = s == (sort s)
                  
                  
task2 :: Int
task2 = do let l_range = read ((splitOn "-" input)!!0) :: Int
           let h_range = read ((splitOn "-" input)!!1) :: Int
           let nums = [n | n <- [l_range..h_range],
                           cd (show n),
                           increasing (show n)]
           length nums


-- Checks if there are any repeated numbers in group size > 2
cd :: [Char] -> Bool
cd s = do let sublist = [sub | sub <- subsequences s, containsDouble' sub, (length sub) >= 2]
          (length $ fil $ sort sublist) > 0 


-- Checks if string does not contain doubles      
containsDouble' :: [Char] -> Bool
containsDouble' []                        = True
containsDouble' (x:xs) | (length xs) > 0  = (x == (head xs)) && (containsDouble' xs)  
                       | otherwise        = True   

            
fil :: [[Char]] -> [[Char]]
fil []                       = []
fil (x:xs) | (length xs) > 0 = fil (filter (\x -> (head x) /= (head (head xs))) (x:xs))
           | otherwise       = [x]
           