module MapReduce where

import Control.Parallel (pseq)
import Control.Parallel.Strategies (Strategy, parMap, using)

simpleMapReduce :: (a -> b) -> ([b] -> c) -> [a] -> c
simpleMapReduce mapF reduceF = reduceF . map mapF

mapReduce :: Strategy b -> (a -> b)
          -> Strategy c -> ([b] -> c)
          -> [a] -> c
mapReduce mapS mapF reduceS reduceF input =
    mapResult `pseq` reduceResult
    where mapResult = parMap mapS mapF input
          reduceResult = reduceF mapResult `using` reduceS

