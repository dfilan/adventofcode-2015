import System.IO
import Data.List

listParensToFloor        :: [Char] -> Int
listParensToFloor []     = 0
listParensToFloor (x:xs) = case x of
                            '(' -> 1 + listParensToFloor xs
                            ')' -> (0-1) + listParensToFloor xs
                            _   -> listParensToFloor xs

firstPrefix :: (Eq a) => [a] -> ([a] -> Bool) -> [a]
firstPrefix list pred
  | good_sublists == [] = []
  | otherwise           =  head $ good_sublists
  where good_sublists = filter pred $ inits list


firstPositionToNthFloor            :: [Char] -> Int -> Int
firstPositionToNthFloor list floor = length $ firstPrefix list $
                                       (\x -> listParensToFloor x == floor)

main = do
  parens_file <- openFile "parens_input.txt" ReadMode
  listParens  <- hGetContents parens_file
  let floor = listParensToFloor listParens
  putStrLn "first part:"
  print floor
  putStrLn "second part:"
  let pos = firstPositionToNthFloor listParens (-1)
  print pos
  hClose parens_file
