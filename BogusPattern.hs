data Fruit = Apple | Orange deriving Show

apple = "apple"
orange = "orange"

whichFruit :: String -> Fruit
whichFruit f = case f of
               "apple"  -> Apple
               "orange" -> Orange
