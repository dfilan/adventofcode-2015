-- see commit b632f5c for version that worked for part 1
import Data.Char
import Data.List
import Data.Function
import System.IO

type Light = (Int, Int, Int)
type Pos = (Int, Int)
type Grid = [Light]

turnOn :: Light -> Light
turnOn (a,b,c) = (a,b,c + 1)

turnOff :: Light -> Light
turnOff (a,b,c) = (a,b,max 0 (c-1))

toggle :: Light -> Light
toggle (a,b,c) = (a,b,c + 2)

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

stringToPos :: String -> Pos
stringToPos str = (read (list!!0), read (list!!2))
    where list = groupBy (on (==) isDigit) str

isDigitOrCom :: Char -> Bool
isDigitOrCom = (`elem` "1234567890,")

hasDigitOrCom :: String -> Bool
hasDigitOrCom = or . map isDigitOrCom

stringToListPos :: String -> [Pos]
stringToListPos = map stringToPos . filter hasDigitOrCom
                       . groupBy (on (==) isDigitOrCom)

stringToGridFunc :: String -> Grid -> Grid
stringToGridFunc str
 | isPrefixOf "turn on" str  = funcGridRange turnOn pos
 | isPrefixOf "turn off" str = funcGridRange turnOff pos
 | isPrefixOf "toggle" str   = funcGridRange toggle pos
 | otherwise                 = id
 where pos = stringToListPos str

instrToGridFunc :: String -> (Grid -> Grid)
instrToGridFunc str grid = foldl' (\a f -> seq (eval $ f a) (f a)) grid
                             $ map stringToGridFunc $ lines str

eval :: Grid -> ()
eval []           = ()
eval ((a,b,c):xs) = a `seq` b `seq` c `seq` eval xs

myGrid :: Grid
myGrid = [(a,b,0) | a <- [0..999], b <- [0..999]]

-- numOnAtEnd :: String -> Grid -> Int
-- numOnAtEnd str = length . filter (\(a,b,bool) -> bool) . instrToGridFunc str

totalBrightnessAtEnd :: String -> Grid -> Int
totalBrightnessAtEnd str = sum . map (\(a,b,c) -> c) . instrToGridFunc str

main = do
  withFile "instructions.txt" ReadMode (\handle -> do
    instr <- hGetContents handle
    let tb = totalBrightnessAtEnd instr myGrid
    print tb)
