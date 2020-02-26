import Data.List.Split

task1 :: FilePath -> IO [Int]
task1 f = do numFile <- readFile f
             let nums = [read n :: Int | n <- splitOn "," numFile]
             let n_nums = [replaceAt (nums!!(n + 3)) (calculate n nums) nums | n <- [0..length nums], 
                                                                               n `mod` 4 == 0,
                                                                               nums!!n /= 99]
             return $ last n_nums

calculate :: Int -> [Int] -> Int
calculate n nums = case nums!!n of
                   1 -> nums!!(n + 1) + nums!!(n + 2)
                   2 -> nums!!(n + 1) * nums!!(n + 2)


replaceAt :: Int -> a -> [a] -> [a]
replaceAt n val (x:xs) | n == 0    = val:xs
                       | otherwise = x:replaceAt (n - 1) val xs
