import Data.List.Split

task1 :: FilePath -> IO Int
task1 f = do numFile <- readFile f
             let nums = [read n :: Int | n <- splitOn "," numFile]
             return $ head $ solve (replaceAt 2 2 (replaceAt 1 12 nums)) 0
             

solve :: [Int] -> Int -> [Int]
solve l n = case l!!n of 
            1 -> solve (replaceAt (l!!(n + 3)) (calculate (+) n l) l) (n + 4)
            2 -> solve (replaceAt (l!!(n + 3)) (calculate (*) n l) l) (n + 4)
            99 -> l          
          
calculate :: (Int -> Int -> Int) -> Int -> [Int] -> Int
calculate op n l = op (l!!(l!!(n + 1))) (l!!(l!!(n + 2)))  


replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt n val (x:xs) | n == 0    = val:xs
                       | otherwise = x:replaceAt (n - 1) val xs
                       
                       
task2 :: FilePath -> IO Int
task2 f = do numFile <- readFile f
             let nums = [read n :: Int | n <- splitOn "," numFile]
             let n_tries = [solve (replaceAt 2 x (replaceAt 1 y nums)) 0 | x <- [0..100], y <- [0..100]]
             let find = findCorrect n_tries
             return $ 100 * find!!1 + find!!2
             
             
findCorrect :: [[Int]] -> [Int]
findCorrect (x:xs) | head x == 19690720 = x
                   | otherwise          = findCorrect xs