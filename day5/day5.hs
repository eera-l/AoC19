import Data.List.Split

task1 :: FilePath -> IO ()
task1 f = do numFile <- readFile f
             let nums = [read n :: Int | n <- splitOn "," numFile]
             nums_re <- solve nums [] 0
             return ()
             

-- Jätte fint
solve :: [Int] -> [Int] -> Int -> IO [Int]
solve l [] n = case l!!n of 
               1 -> solve (replaceAt (l!!(n + 3)) (calculate (+) n l) l) [] (n + 4)
               2 -> solve (replaceAt (l!!(n + 3)) (calculate (*) n l) l) [] (n + 4)
               3 -> do putStrLn "Enter number: "
                       num <- getLine
                       let numIn = read num :: Int            
                       solve (replaceAt (l!!(n + 1)) numIn l) [] (n + 2)
               4 -> do putStrLn (show (l!!(l!!(n + 1))))
                       solve l [] (n + 2)
               5 -> if (l!!(l!!(n + 1))) /= 0 then solve l [] (l!!(l!!(n + 2))) 
                    else solve l [] (n + 3)
               6 -> if (l!!(l!!(n + 1))) == 0 then solve l [] (l!!(l!!(n + 2))) 
                    else solve l [] (n + 3)
               7 -> if (l!!(l!!(n + 1))) < (l!!(l!!(n + 2))) then 
                       solve (replaceAt (l!!(n + 3)) 1 l) [] (n + 4) 
                    else solve (replaceAt (l!!(n + 3)) 0 l) [] (n + 4)
               8 -> if (l!!(l!!(n + 1))) == (l!!(l!!(n + 2))) then 
                        solve (replaceAt (l!!(n + 3)) 1 l) [] (n + 4) 
                    else solve (replaceAt (l!!(n + 3)) 0 l) [] (n + 4)
               99 -> do return l 
               _  -> solve l (readOpcode (l!!n)) n
solve l op n = case l!!n of                
               99 -> do return l 
               x  -> case op!!0 of
                     1 -> solve (replaceAt (l!!(n + 3)) ((readNum l (op!!1) (n + 1)) + (readNum l (op!!2) (n + 2))) l) [] (n + 4)
                     2 -> solve (replaceAt (l!!(n + 3)) ((readNum l (op!!1) (n + 1)) * (readNum l (op!!2) (n + 2))) l) [] (n + 4)
                     4 -> do putStrLn (show (readNum l (op!!1) (n + 1)))
                             solve l [] (n + 2)
                     5 -> if (readNum l (op!!1) (n + 1)) /= 0 then solve l [] (readNum l (op!!2) (n + 2)) 
                          else solve l [] (n + 3)
                     6 -> if (readNum l (op!!1) (n + 1)) == 0 then solve l [] (readNum l (op!!2) (n + 2)) 
                          else solve l [] (n + 3)
                     7 -> if (readNum l (op!!1) (n + 1)) < (readNum l (op!!2) (n + 2)) then 
                          solve (replaceAt (l!!(n + 3)) 1 l) [] (n + 4) 
                          else solve (replaceAt (l!!(n + 3)) 0 l) [] (n + 4)
                     8 -> if (readNum l (op!!1) (n + 1)) == (readNum l (op!!2) (n + 2)) then 
                          solve (replaceAt (l!!(n + 3)) 1 l) [] (n + 4) 
                          else solve (replaceAt (l!!(n + 3)) 0 l) [] (n + 4)
                     _ -> solve l [] (n + 2)
                                      


-- Generic calculation function for readability     
calculate :: (Int -> Int -> Int) -> Int -> [Int] -> Int
calculate op n l = op (l!!(l!!(n + 1))) (l!!(l!!(n + 2)))  


-- The second argument decides the way the number
-- is read from the list
readNum :: [Int] -> Int -> Int -> Int
readNum l op n = case op of
                 0 -> l!!(l!!n)
                 1 -> l!!n


-- Replace value at given index in list
replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt n val (x:xs) | n == 0    = val:xs
                       | otherwise = x:replaceAt (n - 1) val xs
                       

-- Read the given opcode                      
readOpcode :: Int -> [Int]
readOpcode n = do let op = n `mod` 100
                  let f_par = (n `div` 100) `mod` 10
                  let s_par = if (n `div` 1000) >= 10 then (n `div` 1000) `mod` 10 else (n `div` 1000)
                  let t_par = if (n `div` 1000) >= 10 then (n `div` 10000) else 0
                  [op, f_par, s_par, t_par]
                  
               
task2 :: FilePath -> IO ()
task2 f = do numFile <- readFile f
             let nums = [read n :: Int | n <- splitOn "," numFile]
             nums_re <- solve nums [] 0
             return ()
                              