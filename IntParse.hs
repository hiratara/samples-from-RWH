import Data.Char (digitToInt)

asInt :: String -> Int
asInt xs = loop 0 xs
    where loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                            in loop acc' xs
          loop acc [] = acc

