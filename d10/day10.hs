import Data.List

digits :: Int -> [Int]
digits x
 | x < 10    = [x]
 | otherwise = (digits $ div x 10) ++ [mod x 10]

lookAndSay :: [Int] -> [Int]
lookAndSay = concat . map (\x -> (digits $ length x) ++ [head x]) . group

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 f x = x
applyNTimes n f x = f $ applyNTimes (n - 1) f x
