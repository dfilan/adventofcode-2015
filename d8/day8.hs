import Data.List
import System.IO

innards :: [a] -> [a]
innards = init . tail

pairs :: [a] -> [[a]]
pairs []       = []
pairs [x]      = []
pairs (x:y:ys) = [x,y]:(pairs (y:ys))

firstTwo :: [a] -> [a]
firstTwo []       = []
firstTwo [x]      = [x]
firstTwo (x:y:ys) = [x,y]

keepSlashes :: String -> String
keepSlashes ""  = ""
keepSlashes [x] = ""
keepSlashes str
 | firstTwo str == "\\\\" = "\\\\" ++ (keepSlashes $ tail $ tail str)
 | otherwise              = keepSlashes $ tail str

numSlash :: String -> Int
numSlash = (`div` 2) . length . keepSlashes

removeSlashes :: String -> String
removeSlashes ""  = ""
removeSlashes [x] = [x]
removeSlashes str
 | firstTwo str == "\\\\" = removeSlashes $ tail $ tail str
 | otherwise              = (head str):(removeSlashes $ tail str)

numHex :: String -> Int
numHex = length . filter (\x -> x == "\\x") . pairs . removeSlashes

numQuote :: String -> Int
numQuote = length . filter (\x -> x == "\\\"") . pairs . removeSlashes

extraChars :: String -> Int
extraChars str = 2 + 3 * numHex ins + numQuote ins + numSlash ins
    where ins = innards str

main = do
  withFile "code.txt" ReadMode (\handle -> do
    code <- hGetContents handle
    let lnc = lines code
    print $ sum $ map extraChars $ lnc
    -- print $ extraChars $ lnc!!41
    )
