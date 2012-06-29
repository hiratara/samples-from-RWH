import Data.Char (toUpper)

isGreen :: IO Bool
isGreen = do
  putStrLn "Is green your favorite color?"
  inpStr <- getLine
  return ((toUpper $ head inpStr) == 'Y')

main :: IO ()
main = do
  str <- isGreen
  putStrLn $ show str
