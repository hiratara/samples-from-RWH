import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do
  inh <- openFile "/tmp/input.txt" ReadMode
  outh <- openFile "/tmp/output.txt" WriteMode
  inpStr <- hGetContents inh
  hPutStr outh (processData inpStr)
  hClose inh
  hClose outh

processData :: String -> String
processData = map toUpper
