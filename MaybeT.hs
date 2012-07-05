{-# LANGUAGE UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}
module MaybeT where

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad ()

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
-- (MaybeT mm) `bindMT` f = MaybeT $ do
--                            m <- mm
--                            case m of
--                              Nothing -> return Nothing
--                              Just x -> runMaybeT . f $ x
(MaybeT mm) `bindMT` f = MaybeT $ mm >>= maybe (return Nothing) (runMaybeT . f)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance (Monad m) => Monad (MaybeT m) where
    return = returnMT
    (>>=) = bindMT
    fail = failMT

instance MonadTrans MaybeT where
    -- lift m = MaybeT (Just `liftM` m)
    lift m = MaybeT $ return <$> m
        where (<$>) = liftM

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

instance (MonadState s m) => MonadState s (MaybeT m) where
    get = lift get
    put = lift . put

instance (MonadWriter s m) => MonadWriter s (MaybeT m) where
    tell = lift . tell
    -- listen (MaybeT m) = MaybeT $ do
    --                       (a, w) <- listen m
    --                       case a of
    --                         Just a' -> return $ Just (a', w)
    --                         Nothing -> return Nothing
    listen (MaybeT m) = MaybeT $ do
                          (a, w) <- listen m
                          case a of
                            Just a' -> return $ Just (a', w)
                            Nothing -> return Nothing
    pass (MaybeT m) = MaybeT $ do
                        x <- m
                        case x of
                          Just x' -> (pass $ return x') >>= (return . Just)
                          Nothing -> return Nothing
