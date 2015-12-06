import Data.Char
import Data.List
import System.IO

prods :: (Num a) => [a] -> [a]
prods = map product . filter (\x -> length x == 2) . subsequences

totalSA :: (Num a) => [a] -> a
totalSA = (*) 2 . sum . prods

minFace :: (Num a, Ord a) => [a] -> a
minFace = minimum . prods

paperNeeded :: (Num a, Ord a) => [a] -> a
paperNeeded list = (totalSA list) + (minFace list)

totalPaper :: (Num a, Ord a) => [[a]] -> a
totalPaper = sum . map paperNeeded

-- this string to int program will only work for strings like "19"
stringToInt :: String -> Int
stringToInt = sum . zipWith (*) [10^n | n <- [0..]]
                . reverse . map digitToInt
                     
sameSort :: Char -> Char -> Bool
sameSort a b = (and digits) || not (or digits)
    where digits = map isHexDigit [a,b]

groupDigits :: String -> [String]
groupDigits = filter (or . map isHexDigit) . groupBy sameSort

strToDigits :: String -> [Int]
strToDigits = map stringToInt . groupDigits

fileToTotalPaper :: String -> Int
fileToTotalPaper = totalPaper . map strToDigits . lines

main = do
  withFile "box_list.txt" ReadMode (\handle -> do
      contents <- hGetContents handle
      let paper = fileToTotalPaper contents
      print paper)
