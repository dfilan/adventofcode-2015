import Data.Char
import Data.List
import System.IO

isVowel :: Char -> Bool
isVowel char = elem char "aeiou"

isVowely :: String -> Bool
isVowely str = length (filter isVowel str) > 2

isDoubly :: String -> Bool
isDoubly str = length (filter (\x -> length x > 1) $ group str) > 0

isNaughty :: String -> Bool
isNaughty str = or $ map (`isInfixOf` str) ["ab", "cd", "pq", "xy"]

isNice :: String -> Bool
isNice = and . (<*>) [isVowely, isDoubly, not . isNaughty] . pure

-- isExtraDoubly :: String -> Bool
-- isExtraDoubly str = 

numNice :: String -> Int
numNice = length . filter isNice . lines

main = do
  withFile "strings.txt" ReadMode (\handle -> do
    strings <- hGetContents handle
    let numnice = numNice strings
    print numnice)
