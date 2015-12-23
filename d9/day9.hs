import Data.List
import Data.Char
import System.IO

noCrap :: [String] -> [String]
noCrap = concat . map
         (filter (\x -> not ((isNumber $ head x) || x == "to" || x == "="))
         . words)

cities :: [String] -> [String]
cities file = nub $ (names \\ (nub $ names))
    where names = noCrap file

lengthBtwn :: [String] -> (String, String) -> Int
lengthBtwn dirs pair = read $ last $ words $ head $ filter
                        (\x -> (isInfixOf (fst pair) x)
                               && (isInfixOf (snd pair) x))
                        dirs

pairs :: [a] -> [(a,a)]
pairs []       = []
pairs [x]      = []
pairs (x:y:ys) = (x,y):(pairs (y:ys))

pathToLength :: [String] -> [String] -> Int
pathToLength dirs = sum . map (lengthBtwn dirs) . pairs

lenSmallestPath :: [String] -> Int
lenSmallestPath dirs = minimum $ map (pathToLength dirs) $ permutations
                       $ cities dirs

lenLongestPath :: [String] -> Int
lenLongestPath dirs = maximum $ map (pathToLength dirs) $ permutations
                      $ cities dirs

main = do
  withFile "cities.txt" ReadMode (\handle -> do
    file <- hGetContents handle
    let directions = lines file
    print $ lenLongestPath directions
    )
