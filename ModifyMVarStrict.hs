{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception (SomeException, block, catch, throw, unblock)
import Prelude hiding (catch) -- use Control.Exception's version

modifyMVar_strict :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_strict m io = block $ do
  a <- takeMVar m
  !b <- unblock (io a) `catch` \(e::SomeException) ->
        putMVar m a >> throw e
  putMVar m b
