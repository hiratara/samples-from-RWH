{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.State

newtype Supply s a = S (State [s] a)
    deriving (Monad)

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

runSupply (S m) xs = runState m xs
