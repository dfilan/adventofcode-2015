import Data.Complex
import Data.List
import System.IO
import Control.Applicative

charToComplex :: (RealFloat a) => Char -> Complex a
charToComplex char = case char of
                      '>' -> 1 :+ 0
                      '^' -> 0 :+ 1
                      '<' -> (-1) :+ 0
                      'v' -> 0 :+ (-1)
                      _   -> 0 :+ 0

numsVisited :: (RealFloat a) => String -> [Complex a]
numsVisited = map (sum . map charToComplex) . inits
       
diffNumsVisited :: String -> Int
diffNumsVisited = length . nub . numsVisited

odds :: [a] -> [a]
odds list = [list!!n | n <- [1,3..(length list)]]

evens :: [a] -> [a]
evens list = [list!!n | n <- [0,2..(length list) - 1]]

santaVisits :: (RealFloat a) => String -> [Complex a]
santaVisits = nub . numsVisited . evens

roboVisits :: (RealFloat a) => String -> [Complex a]
roboVisits = nub . numsVisited . odds

totalVisits :: String -> Int
totalVisits =  length . (union <$> santaVisits <*> roboVisits)

main = do
    withFile "grid_movements.txt" ReadMode (\handle -> do
      contents <- hGetContents handle
      let houses_visited = diffNumsVisited contents
      putStrLn "part 1:"
      print houses_visited
      let robo_visited = totalVisits contents
      putStrLn "part 2:"
      print robo_visited)
