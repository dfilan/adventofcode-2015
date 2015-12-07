import Data.Char
import Data.List
import System.IO

type Light = (Int, Int, Bool)
type Pos = (Int, Int)
type Grid = [Light]

turnOn :: Light -> Light
turnOn (a,b,c) = (a,b,True)

turnOff :: Light -> Light
turnOff (a,b,c) = (a,b,False)

toggle :: Light -> Light
toggle (a,b,c) = (a,b,not c)

fstl :: Light -> Int
fstl (a,_,_) = a

sndl :: Light -> Int
sndl (_,b,_) = b

inRange :: Pos -> Pos -> Light -> Bool
inRange tup1 tup2 = and . (<*>) [(>= fst tup1) . fstl,
                                 (<= fst tup2) . fstl,
                                 (>= snd tup1) . sndl,
                                 (<= snd tup2) . sndl] . pure

funcInRange :: (Light -> Light) -> Pos -> Pos -> Light -> Light
funcInRange func tup1 tup2 light
 | inRange tup1 tup2 light = func light
 | otherwise               = light

funcGridRange :: (Light -> Light) -> [Pos] -> Grid -> Grid
funcGridRange func list = map (funcInRange func (list!!0) (list!!1))

stringToInt :: String -> Int
stringToInt = sum . zipWith (*) [10^n | n <- [0..]]
                . reverse . map digitToInt

sameTest :: (Char -> Bool) -> Char -> Char -> Bool
sameTest test a b = (and $ map test [a,b]) || (not $ or $ map test [a,b])

stringToPos :: String -> Pos
stringToPos str = (stringToInt (list!!0), stringToInt (list!!2))
    where list = groupBy (sameTest isDigit) str

isDigitOrCom :: Char -> Bool
isDigitOrCom = (`elem` "1234567890,")

hasDigitOrCom :: String -> Bool
hasDigitOrCom = or . map isDigitOrCom

stringToListPos :: String -> [Pos]
stringToListPos = map stringToPos . filter hasDigitOrCom
                       . groupBy (sameTest isDigitOrCom)

stringToGridFunc :: String -> Grid -> Grid
stringToGridFunc str
 | isPrefixOf "turn on" str  = funcGridRange turnOn pos
 | isPrefixOf "turn off" str = funcGridRange turnOff pos
 | isPrefixOf "toggle" str   = funcGridRange toggle pos
 | otherwise                 = id
 where pos = stringToListPos str

listApply :: [a -> a] -> a -> a
listApply []     = id
listApply (f:fs) =  listApply fs . f

instrToGridFunc :: String -> (Grid -> Grid)
instrToGridFunc = listApply . map stringToGridFunc . lines 

myGrid :: Grid
myGrid = [(a,b,False) | a <- [0..999], b <- [0..999]]

numOnAtEnd :: String -> Grid -> Int
numOnAtEnd str = length . filter (\(a,b,bool) -> bool) . instrToGridFunc str

-- numLightsOn :: Grid -> Int
-- numLightsOn = length . filter (\(a,b,bool) -> bool)

main = do
  withFile "instructions.txt" ReadMode (\handle -> do
    instr <- hGetContents handle
    putStrLn "part 1"
    let numon = numOnAtEnd instr myGrid
    print numon
    )
