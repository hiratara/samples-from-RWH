import Test.QuickCheck
import Prettify2
import Control.Monad (liftM, liftM2)

-- instance Arbitrary Doc where
--     arbitrary = do
--       n <- choose (1, 6) :: Gen Int
--       case n of
--         1 -> return Empty
--         2 -> do x <- arbitrary
--                 return (Char x)
--         3 -> do x <-arbitrary
--                 return (Text x)
--         4 -> return Line
--         5 -> do x <- arbitrary
--                 y <- arbitrary
--                 return (Concat x y)
--         6 -> do x <- arbitrary
--                 y <- arbitrary
--                 return (Union x y)
--         _ -> error ""

instance Arbitrary Doc where
    arbitrary = oneof [
                 return Empty,
                 liftM Char arbitrary,
                 liftM Text arbitrary,
                 return Line,
                 liftM2 Concat arbitrary arbitrary,
                 liftM2 Union arbitrary arbitrary
                ]

prop_empty_id x = empty <> x == x
                  &&
                  x <> empty == x

prop_char c = char c == Char c
prop_text s = text s == if null s then Empty else Text s
prop_line = line == Line
prop_double d = double d == text (show d)

