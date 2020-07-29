import Data.Char(digitToInt)
import Data.List.Split
import Data.List


width :: Int
width = 25


height :: Int
height = 6

 
task1 :: FilePath -> IO Int
task1 f = do digits <- readFile f
             let nums     = map digitToInt digits
             let fullList = readLayerz (readLinez nums width) height
             return $ countOneTwoz fullList $ returnIndex $ findLeastZeros fullList
             
             
-- Divide array into chunks of size n
readLinez :: [a] -> Int -> [[a]]
readLinez [] _ = error "Empty list"
readLinez l 0  = error "Enter number greater than 0"
readLinez l n  = chunksOf n l


-- Divide bidimensional array into chunks of size n
readLayerz :: [[a]] -> Int -> [[[a]]]
readLayerz [[]] _ = error "Empty list"
readLayerz l 0    = error "Enter number greater than 0"
readLayerz l n    = chunksOf n l


-- Return list of tuples with number of zeroes and layer index
findLeastZeros :: [[[Int]]] -> [(Int, Int)]
findLeastZeros [[[]]] = error "Empty list"
findLeastZeros l      = [(sum [length (filter (0==) line) | line <- l!!i], i) | i <- [0..(length l) - 1]]


-- Return index of layer with least zeroes
returnIndex :: [(Int, Int)] -> Int
returnIndex [] = error "Empty list"
returnIndex l  = head [snd x | x <- l, fst x == (minimum (fst $ unzip l))]


-- Multiply number of 1s and 2s in that layer
countOneTwoz :: [[[Int]]] -> Int -> Int
countOneTwoz [[[]]] _ = error "Empty list"
countOneTwoz l n      = sum [length (filter (1==) line) | line <- l!!n] * sum [length (filter (2==) line) | line <- l!!n]


task2 :: FilePath -> IO ()
task2 f = do digits <- readFile f
             let nums      = map digitToInt digits
             let fullList  = transpose $ readLinez nums (width * height)
             let colorList = [colorize num | num <- fullList]
             mapM_ print $ readLinez colorList width


-- Turn numbers into characters to produce written solution
colorize :: [Int] -> Char
colorize []                  = error "Empty list"
colorize (x:xs)  | x == 0    = ' '
                 | x == 1    = 'o'
                 | otherwise = colorize xs                  