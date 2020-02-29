import Data.List.Split
import Data.List
import Data.Maybe

task1 :: FilePath -> IO Int
task1 f = do numFile <- readFile f
             let wires = lines numFile 
             let wirePaths = [splitOn "," wire | wire <- wires]
             let fw = wirePaths!!0
             let sw = wirePaths!!1
             let fp = filter (\x -> fst(x) /= 0 && snd(x) /= 0) (buildseq fw [(0,0)])
             let sp = filter (\x -> fst(x) /= 0 && snd(x) /= 0) (buildseq sw [(0,0)])
             let distances = [abs((fst(x) - 0)) + abs((snd(x) - 0)) | x <- filterelem fp sp]
             return $ minimum distances

buildseq :: [[Char]] -> [(Int,Int)] -> [(Int,Int)]
buildseq [] seq     = seq
buildseq (x:xs) seq = buildseq xs (addPoints (fst(command)) (snd(command)) seq)
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
                                   
filterelem :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
filterelem l l' = filter (\x -> x `elem` l') l

task2 :: FilePath -> IO Int
task2 f = do numFile <- readFile f
             let wires = lines numFile 
             let wirePaths = [splitOn "," wire | wire <- wires]
             let fw = wirePaths!!0
             let sw = wirePaths!!1
             let fp = reverse (buildseq fw [(0,0)])
             let sp = reverse (buildseq sw [(0,0)])
             let steps = [(fromJust $ elemIndex x fp) + (fromJust $ elemIndex x sp) | x <- filterelem fp sp]
             return $ minimum $ filter (\x -> x /= 0) steps
                