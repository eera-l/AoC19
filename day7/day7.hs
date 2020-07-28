import Data.List.Split
import Data.List
import Data.IORef

--resolve :: [Int]
--resolve = do numFile <- readFile "input.txt"
             --let nums = [read n :: Int | n <- splitOn "," numFile]
             --counter <- makeCounter
             --nums_re <- [[ 1 | x <- [0..4]] | p <- perm]
             --perm
             
perm :: [[Int]]
perm = permutations [0, 1, 2, 3, 4]


type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = do r <- newIORef (0 :: Int)
                 return (\i -> do modifyIORef r (+i) 
                                  readIORef r)          

solve :: [Int] -> [Int] -> Int -> Counter -> IO Int
solve l [] n c = case l!!n of 
               1 -> solve (replaceAt (l!!(n + 3)) (calculate (+) n l) l) [] (n + 4) c
               2 -> solve (replaceAt (l!!(n + 3)) (calculate (*) n l) l) [] (n + 4) c
               3 -> do c 1
                       if c == (makeCounter $ 1) then solve (replaceAt (l!!(n + 1)) 322 l) [] (n + 2) c
                       else solve (replaceAt (l!!(n + 1)) 322 l) [] (n + 2) c
               4 -> do putStrLn (show (l!!(l!!(n + 1))))
                       solve l [] (n + 2) c
               5 -> if (l!!(l!!(n + 1))) /= 0 then solve l [] (l!!(l!!(n + 2)))  c
                    else solve l [] (n + 3) c
               6 -> if (l!!(l!!(n + 1))) == 0 then solve l [] (l!!(l!!(n + 2))) c
                    else solve l [] (n + 3) c
               7 -> if (l!!(l!!(n + 1))) < (l!!(l!!(n + 2))) then 
                       solve (replaceAt (l!!(n + 3)) 1 l) [] (n + 4) c
                    else solve (replaceAt (l!!(n + 3)) 0 l) [] (n + 4) c
               8 -> if (l!!(l!!(n + 1))) == (l!!(l!!(n + 2))) then 
                        solve (replaceAt (l!!(n + 3)) 1 l) [] (n + 4) c
                    else solve (replaceAt (l!!(n + 3)) 0 l) [] (n + 4) c
               99 -> do c (-2)
                        return (l!!(l!!(n + 1))) 
               _  -> solve l (readOpcode (l!!n)) n c
solve l op n c = case l!!n of                
               99 -> do c (-2)
                        return (l!!(l!!(n + 1))) 
               x  -> case op!!0 of
                     1 -> solve (replaceAt (l!!(n + 3)) ((readNum l (op!!1) (n + 1)) + (readNum l (op!!2) (n + 2))) l) [] (n + 4) c
                     2 -> solve (replaceAt (l!!(n + 3)) ((readNum l (op!!1) (n + 1)) * (readNum l (op!!2) (n + 2))) l) [] (n + 4) c
                     4 -> do putStrLn (show (readNum l (op!!1) (n + 1)))
                             solve l [] (n + 2) c
                     5 -> if (readNum l (op!!1) (n + 1)) /= 0 then solve l [] (readNum l (op!!2) (n + 2))  c
                          else solve l [] (n + 3) c
                     6 -> if (readNum l (op!!1) (n + 1)) == 0 then solve l [] (readNum l (op!!2) (n + 2)) c
                          else solve l [] (n + 3) c
                     7 -> if (readNum l (op!!1) (n + 1)) < (readNum l (op!!2) (n + 2)) then 
                          solve (replaceAt (l!!(n + 3)) 1 l) [] (n + 4) c 
                          else solve (replaceAt (l!!(n + 3)) 0 l) [] (n + 4) c
                     8 -> if (readNum l (op!!1) (n + 1)) == (readNum l (op!!2) (n + 2)) then 
                          solve (replaceAt (l!!(n + 3)) 1 l) [] (n + 4) c
                          else solve (replaceAt (l!!(n + 3)) 0 l) [] (n + 4) c
                     _ -> solve l [] (n + 2) c
                                      
            
calculate :: (Int -> Int -> Int) -> Int -> [Int] -> Int
calculate op n l = op (l!!(l!!(n + 1))) (l!!(l!!(n + 2)))  

readNum :: [Int] -> Int -> Int -> Int
readNum l op n = case op of
                 0 -> l!!(l!!n)
                 1 -> l!!n


replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt n val (x:xs) | n == 0    = val:xs
                       | otherwise = x:replaceAt (n - 1) val xs
                       
                       
readOpcode :: Int -> [Int]
readOpcode n = do let op = n `mod` 100
                  let f_par = (n `div` 100) `mod` 10
                  let s_par = if (n `div` 1000) >= 10 then (n `div` 1000) `mod` 10 else (n `div` 1000)
                  let t_par = if (n `div` 1000) >= 10 then (n `div` 10000) else 0
                  [op, f_par, s_par, t_par]
                  