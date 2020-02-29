task1 :: FilePath -> IO Int
task1 f = do numFile <- readFile f
             let nums = lines numFile
             let numbers = [calculateCost (read n :: Int) | n <- nums]
             return $ sum numbers

calculateCost :: Int -> Int
calculateCost num = num `div` 3 - 2                 
                  
                  
task2 :: FilePath -> IO Int
task2 f = do numFile <- readFile f
             let nums = lines numFile
             let numbers = [calculateCostRe (read n :: Int) | n <- nums]
             return $ sum numbers
             
             
calculateCostRe :: Int -> Int
calculateCostRe num | num' <= 0  = 0
                    | otherwise  = num' + calculateCostRe num'
              where num' = num `div` 3 - 2 
              