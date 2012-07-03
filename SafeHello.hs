module SafeHello where 

import MonadHandle
import System.IO (IOMode(..))

safeHello :: MonadHandle m h => FilePath -> m ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h
