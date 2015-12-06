import Data.Char
import Crypto.Hash
import qualified Data.ByteString.Char8 as BS

md5 :: BS.ByteString -> Digest MD5
md5 = hash

zeroy :: BS.ByteString -> Bool
zeroy str
 | BS.length str < 5 = False
 | otherwise         = BS.take 5 str == BS.pack "00000"

-- only use for positive integers
revNumToStr :: Int -> String
revNumToStr n
 | n < 10    = [intToDigit n]
 | otherwise = intToDigit (mod n 10) : revNumToStr (div n 10)

numToStr :: Int -> String
numToStr = reverse . revNumToStr

hashStrNum :: String -> Int -> Digest MD5
hashStrNum str n = md5 $ BS.pack $ str ++ numToStr n

leastZeroy :: String -> Int
leastZeroy str = head $ filter (zeroy . BS.take 5 . digestToHexByteString
                                . hashStrNum str) [0..] 

main = do
  print $ leastZeroy "iwrupvqb"
