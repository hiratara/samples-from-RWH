module Logger ( Logger, Log, runLogger, record ) where

newtype Logger a = Logger { execLogger :: (a, Log) }

type Log = [String]

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
    return a = Logger (a, [])
    (Logger m) >>= k = let (a, w) = m
                           Logger n = k a
                           (b, x) = n
                       in Logger (b, w ++ x)

