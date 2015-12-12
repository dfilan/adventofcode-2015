import Data.List
import Data.Char
import System.IO

type Signal  = [Bool]
type Wire    = (String, Signal)
type Circuit = [Wire]

lshift :: Signal -> Int -> Signal
lshift sig int = (drop int sig) ++ (take int $ repeat False)

rshift :: Signal -> Int -> Signal
rshift sig int = (take int $ repeat False) ++ (take negint sig)
    where negint = (length sig) - int

intToSig :: Int -> Signal
intToSig int = [(int `div` (2^n)) `mod` 2 == 1 | n <- [15,14..0]]

sigToInt :: Signal -> Int
sigToInt [] = 0
sigToInt (x:xs)
 | x         = (2^len) + sigToInt xs
 | otherwise = sigToInt xs
 where len = length xs

nameToSig :: String -> Circuit -> Signal
nameToSig name = snd . head . filter (\x -> fst x == name)

bitOp :: (Bool -> Bool -> Bool) -> String -> Circuit -> Circuit
bitOp op string circ
 | isDigit $ head $ wrds!!0 = ((wrds!!4), zipWith op
                               (intToSig (read (wrds!!0)))
                               (nameToSig (wrds!!2) circ)):circ
 | isDigit $ head $ wrds!!2 = ((wrds!!4), zipWith op
                               (nameToSig (wrds!!0) circ)
                               (intToSig (read (wrds!!2)))):circ
 | otherwise                = ((wrds!!4), zipWith op
                               (nameToSig (wrds!!0) circ)
                               (nameToSig (wrds!!2) circ)):circ
 where wrds = words string

stringToCircFunc :: String -> Circuit -> Circuit
stringToCircFunc string circ
 | wrds!!0 == "NOT"       = ((wrds!!3),
                             map not $ nameToSig (wrds!!1) circ):circ
 | wrds!!1 == "AND"       = bitOp (&&) string circ
 | wrds!!1 == "OR"        = bitOp (||) string circ
 | wrds!!1 == "LSHIFT"    = ((wrds!!4), lshift (nameToSig (wrds!!0) circ)
                                        (read (wrds!!2))):circ
 | wrds!!1 == "RSHIFT"    = ((wrds!!4), rshift (nameToSig (wrds!!0) circ)
                                        (read (wrds!!2))):circ
 | isDigit $ head wrds!!0 = ((wrds!!2), intToSig $ read (wrds!!0)):circ
 | otherwise              = ((wrds!!2), nameToSig (wrds!!0) circ):circ
 where wrds = words string

instrToCircFunc :: [String] -> Circuit -> Circuit
instrToCircFunc instr circ = foldl (\a f -> f a) circ
                               $ map stringToCircFunc instr

wordsToArrow :: String -> [String]
wordsToArrow = fst . break (== "->") . words

before :: String -> String -> Bool
before x y = ((length x) < (length y)) || ((length x == length y) && x < y)

mySort :: [String] -> [String]
mySort []     = []
mySort (x:xs) = mySort (filter (\y -> (last $ words y) `before`
                                      (last $ words x)) xs) ++ [x]
                ++ mySort (filter (\y -> (last $ words x) `before`
                                         (last $ words y)) xs)

flipList :: [a] -> [a]
flipList []     = []
flipList (x:xs) = xs ++ [x]

main = do
  withFile "circuit_instructions.txt" ReadMode (\handle -> do
    instr <- hGetContents handle
    let sortedInstr = flipList $ mySort $ lines instr
    -- print sortedInstr
    let endCirc = instrToCircFunc sortedInstr []
        siga = nameToSig "a" endCirc
        intsiga = sigToInt siga
    print intsiga
    -- print endCirc
    )
