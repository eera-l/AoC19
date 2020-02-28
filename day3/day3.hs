import Data.List.Split

data Point = Point Int Int 
 deriving (Show, Eq)
 
data Wire = Empty | Sequence Point Wire
  deriving (Show, Eq)

task1 :: FilePath -> IO Int
task1 f = do numFile <- readFile f
             let wires = lines numFile 
             let wirePaths = [splitOn "," wire | wire <- wires]
             let firstWire = wirePaths!!0
             let secondWire = wirePaths!!1
             let fWire = buildWire firstWire Empty
             let sWire = buildWire secondWire Empty
             let firstPoints = filter (\x -> fst(x) /= 0 && snd(x) /= 0) (pointsToList fWire)
             let secondPoints = filter (\x -> fst(x) /= 0 && snd(x) /= 0) (pointsToList sWire)
             let distances = [abs((fst(x) - 0)) + abs((snd(x) - 0)) | x <- firstPoints,
                                                                      y <- secondPoints,
                                                                      fst(x) == fst(y) && snd(x) == snd(y)]
             return $ minimum distances
             
-- filter (\x -> fst(x) /= 3000) (findIntersections fWire sWire)
             
             
buildWire :: [[Char]] -> Wire -> Wire
buildWire [] w     = w
buildWire (x:xs) w = buildWire xs (addToWire (fst(splitString x)) (snd(splitString x)) w)
             
splitString :: [Char] -> (Char, Int)
splitString (x:xs) = (x, (read (xs) :: Int))             
             
getCoordinates :: Point -> (Int,Int)
getCoordinates (Point x y) = (x,y)

createWire :: Wire
createWire = Sequence (Point 0 0) Empty 

addToWire :: Char -> Int -> Wire -> Wire
addToWire c n Empty          = addToWire c n createWire
addToWire c n (Sequence p w) | n == 1 = case c of
                                        'L' -> Sequence (Point (fst(getCoordinates p)) ((snd(getCoordinates p)) - 1)) (Sequence p w)
                                        'R' -> Sequence (Point (fst(getCoordinates p)) ((snd(getCoordinates p)) + 1))(Sequence p w)
                                        'U' -> Sequence (Point ((fst(getCoordinates p)) - 1) (snd(getCoordinates p))) (Sequence p w)
                                        'D' -> Sequence (Point ((fst(getCoordinates p)) + 1) (snd(getCoordinates p))) (Sequence p w)
                             | otherwise = case c of
                                        'L' -> addToWire c (n - 1) (Sequence (Point (fst(getCoordinates p)) ((snd(getCoordinates p)) - 1)) (Sequence p w))
                                        'R' -> addToWire c (n - 1) (Sequence (Point (fst(getCoordinates p)) ((snd(getCoordinates p)) + 1)) (Sequence p w))
                                        'U' -> addToWire c (n - 1) (Sequence (Point ((fst(getCoordinates p)) - 1) (snd(getCoordinates p))) (Sequence p w))
                                        'D' -> addToWire c (n - 1) (Sequence (Point ((fst(getCoordinates p)) + 1) (snd(getCoordinates p))) (Sequence p w))
                               
                               
                               
pointsToList :: Wire -> [(Int,Int)]
pointsToList Empty                    = []
pointsToList (Sequence (Point x y) w) = (x, y) : pointsToList w







                               