import System.IO

listParensToFloor        :: [Char] -> Int
listParensToFloor []     = 0
listParensToFloor (x:xs) = case x of
                            '(' -> 1 + listParensToFloor xs
                            ')' -> (0-1) + listParensToFloor xs
                            _   -> listParensToFloor xs

main = do
  parens_file <- openFile "parens_input.txt" ReadMode
  listParens  <- hGetContents parens_file
  let floor = listParensToFloor listParens
  print floor
  hClose parens_file
