import qualified Data.Map as Map

al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]

mapFromAL = Map.fromList al

mapFold = foldl (\map (k, v) -> Map.insert k v map) Map.empty al

mapManual = Map.insert 2 "two" .
            Map.insert 4 "four" .
            Map.insert 1 "one" .
            Map.insert 3 "three" $ Map.empty

