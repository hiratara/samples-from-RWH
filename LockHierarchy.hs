{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent
import NiceFork

nestedModification outer inner = do
  modifyMVar_ outer $ \x -> do
    threadDelay 1
    yield
    modifyMVar_ inner $ \y -> return (y + 1)
    return (x + 1)
  putStrLn "done"

main = do
  manager <- newManager
  (a :: MVar Int) <- newMVar 1
  (b :: MVar Int) <- newMVar 2
  forkManaged manager $ nestedModification a b
  forkManaged manager $ nestedModification b a
  waitAll manager
  -- a <- newMVar 1
  -- b <- newMVar 2
  -- forkIO $ nestedModification a b
  -- forkIO $ nestedModification b a
