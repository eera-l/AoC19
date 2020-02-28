import Data.List.Split
import Data.List
import Data.Maybe

task1 :: FilePath -> IO Int
task1 f = do numFile <- readFile f
             let wires = lines numFile 
             let wirePaths = [splitOn "," wire | wire <- wires]
             let firstWire = wirePaths!!0
             let secondWire = wirePaths!!1
             let firstPoints = filter (\x -> fst(x) /= 0 && snd(x) /= 0) (buildSequence firstWire [(0,0)])
             let secondPoints = filter (\x -> fst(x) /= 0 && snd(x) /= 0) (buildSequence secondWire [(0,0)])
             let distances = [abs((fst(x) - 0)) + abs((snd(x) - 0)) | x <- filterElements firstPoints secondPoints]
             return $ minimum distances

buildSequence :: [[Char]] -> [(Int,Int)] -> [(Int,Int)]
buildSequence [] seq     = seq
buildSequence (x:xs) seq = buildSequence xs (addPoints (fst(command)) (snd(command)) seq)
               where command = splitString x 
            
splitString :: [Char] -> (Char, Int)
splitString (x:xs) = (x, (read (xs) :: Int))

addPoints :: Char -> Int -> [(Int,Int)] -> [(Int,Int)]
addPoints c n []                 = []
addPoints c n (x:xs) | n == 0    = (x:xs)
                     | otherwise = case c of
                                   'L' -> addPoints c (n - 1) ((fst(x), (snd(x) - 1)) : x:xs)
                                   'R' -> addPoints c (n - 1) ((fst(x), (snd(x) + 1)) : x:xs)  
                                   'U' -> addPoints c (n - 1) (((fst(x) - 1), snd(x)) : x:xs)                                   
                                   'D' -> addPoints c (n - 1) (((fst(x) + 1), snd(x)) : x:xs)
                                   
filterElements :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
filterElements l l' = filter (\x -> x `elem` l') l

task2 :: FilePath -> IO Int
task2 f = do numFile <- readFile f
             let wires = lines numFile 
             let wirePaths = [splitOn "," wire | wire <- wires]
             let firstWire = wirePaths!!0
             let secondWire = wirePaths!!1
             let firstPoints = reverse (buildSequence firstWire [(0,0)])
             let secondPoints = reverse (buildSequence secondWire [(0,0)])
             let steps = [(fromJust $ elemIndex x firstPoints) + (fromJust $ elemIndex x secondPoints) | x <- filterElements firstPoints secondPoints]
             return $ minimum $ filter (\x -> x /= 0) steps








                               