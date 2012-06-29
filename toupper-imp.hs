import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do
  inh <- openFile "/tmp/input.txt" ReadMode
  outh <- openFile "/tmp/output.txt" WriteMode
  mainloop inh outh
  hClose inh
  hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = do
  ineof <- hIsEOF inh
  if ineof
     then return ()
     else do
       inpStr <- hGetLine inh
       hPutStrLn outh $ map toUpper inpStr
       mainloop inh outh
