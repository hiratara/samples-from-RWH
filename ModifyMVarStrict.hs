{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, throw, mask)
import Prelude hiding (catch) -- use Control.Exception's version

modifyMVar_strict :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_strict m io = mask $ \restore -> do
  a <- takeMVar m
  !b <- restore (io a) `catch` \(e::SomeException) ->
        putMVar m a >> throw e
  putMVar m b
