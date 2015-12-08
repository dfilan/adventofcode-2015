import Data.Char
import Data.List
import System.IO
import Text.Regex.PCRE

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

isExtraDoubly :: String -> Bool
isExtraDoubly = (=~ "(..).*\\1")

isRepeaty :: String -> Bool
isRepeaty = (=~ "(.).\\1")

isNewNice :: String -> Bool
isNewNice = and . (<*>) [isExtraDoubly, isRepeaty] . pure

numNice :: String -> Int
numNice = length . filter isNice . lines

numNewNice :: String -> Int
numNewNice = length . filter isNewNice . lines

main = do
  withFile "strings.txt" ReadMode (\handle -> do
    strings <- hGetContents handle
    let numnice = numNice strings
    putStrLn "part 1"
    print numnice
    let numnewnice = numNewNice strings
    putStrLn "part 2"
    print numnewnice)
