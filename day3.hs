import Data.Complex
import Data.List
import System.IO

charToComplex :: (RealFloat a) => Char -> Complex a
charToComplex char = case char of
                      '>' -> 1 :+ 0
                      '^' -> 0 :+ 1
                      '<' -> (-1) :+ 0
                      'v' -> 0 :+ (-1)
                      _   -> 0 :+ 0

numsVisited :: (RealFloat a) => String -> [Complex a]
numsVisited = map (sum . map charToComplex) . inits

-- slow, but it gets there
uniques :: (Eq a) => [a] -> [a]
uniques [] = []
uniques (x:xs)
 | elem x xs = uniques xs
 | otherwise = x : uniques xs
       
diffNumsVisited :: String -> Int
diffNumsVisited = length . uniques . numsVisited

main = do
    withFile "grid_movements.txt" ReadMode (\handle -> do
      contents <- hGetContents handle
      let houses_visited = diffNumsVisited contents
      putStrLn "part 1:"
      print houses_visited)

