module DList ( DList, fromList, toList, empty, append, cons, dfoldr ) where

import Data.Monoid

newtype DList a = DL { unDL :: [a] -> [a] }

append :: DList a -> DList a -> DList a
-- append xs ys = DL (unDL xs . unDL ys)
append (DL fx) (DL fy) = DL (fx . fy)

fromList :: [a] -> DList a
fromList xs = DL (xs ++)

toList :: DList a -> [a]
-- toList (DL xs) = xs []
toList xs = unDL xs []

empty :: DList a
empty = DL id

cons :: a -> DList a -> DList a
cons x (DL xs) = DL ((x:) . xs)
infixr `cons`

dfoldr :: (a -> b -> b) -> b -> DList a -> b
dfoldr f z xs = foldr f z (toList xs)

safeHead :: DList a -> Maybe a
safeHead xs = case toList xs of
                (y:_) -> Just y
                _ -> Nothing

dmap :: (a -> b) -> DList a -> DList b
dmap f = dfoldr (\x -> \xs -> cons (f x) xs) empty

instance Functor DList where fmap = dmap

instance Monoid (DList a) where
    mempty = empty
    mappend = append

