import Data.List.Split
import Data.Map as M

task1 :: FilePath -> IO Int
task1 f = do orbitFile <- readFile f
             let orbitsf = lines orbitFile
             let orbitz = [makeTuple $ splitOn ")" or | or <- orbitsf]
             let orbits = [invert orb | orb <- orbitz]
             let mapz = M.fromList(orbits)
             return $ sum [iter (fst(orbs)) mapz | orbs <- orbits]          
             
makeTuple :: [a] -> (a, a)
makeTuple [x,y] = (x,y)

invert :: (a,a) -> (a,a)
invert (x,y) = (y,x)

iter :: [Char] -> Map [Char] [Char] -> Int
iter orb mapz = case looked of
                Just a -> 1 + (iter a mapz)
                Nothing -> 0 
          where looked = M.lookup orb mapz
          
listPath :: [Char] -> Map [Char] [Char] -> [[Char]]
listPath f mapz = case M.lookup f mapz of
                  Just a -> a : listPath a mapz        
                  Nothing -> []                  

findDistance :: [Char] -> [Char] -> Map [Char] [Char] -> Int
findDistance f s mapz = case looked of
                        Just a -> if a /= s then 1 + (findDistance a s mapz)
                                  else 1
                        Nothing -> 0
           where looked = M.lookup f mapz           
          
task2 f = do orbitFile <- readFile f
             let orbitsf = lines orbitFile
             let orbitz = [makeTuple $ splitOn ")" or | or <- orbitsf]
             let orbits = [invert orb | orb <- orbitz]
             let mapz = M.fromList(orbits)
             let lf = listPath "YOU" mapz
             let ls = listPath "SAN" mapz  
             let elems = Prelude.filter (\x -> x `elem` ls) lf             
             let start = mapz ! "YOU"
             let finish = mapz ! "SAN"
             return $ minimum [(findDistance start el mapz) + (findDistance finish el mapz) | el <- elems]
             