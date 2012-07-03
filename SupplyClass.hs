{-# LANGUAGE MultiParamTypeClasses #-}
import Supply

class (Monad m) => MonadSupply s m where
    next :: m (Maybe s)

import qualified Supply as S

instance MonadSupply s (S.Supply s) where
    next = S.next
