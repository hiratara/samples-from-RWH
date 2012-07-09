module NiceFork
    (
      ThreadManager
    , newManager
    , forkManaged
    , getStatus
    , waitFor
    , waitAll
    ) where

import Control.Concurrent
import Control.Exception (SomeException, try)
import qualified Data.Map as M
import Control.Monad (join)

newtype ThreadManager =
    Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
    deriving (Eq)

data ThreadStatus = Running
                  | Finished         -- terminated normally
                  | Threw SomeException  -- killed by uncaught exception
                    deriving (Show)

instance Eq ThreadStatus where
    Running == Running = True
    Finished == Finished = True
    _ == _ = False

newManager :: IO ThreadManager
newManager = Mgr `fmap` newMVar M.empty

forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
    modifyMVar mgr $ \m -> do
      state <- newEmptyMVar
      tid <- forkIO $ do
        result <- try body
        putMVar state (either Threw (const Finished) result)
      return (M.insert tid state m, tid)

getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid =
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
      Nothing -> return (m, Nothing)
      Just st -> do
        mst <- tryTakeMVar st
        case mst of
          Nothing -> return (m, Just Running)
          Just sth -> return (M.delete tid m, Just sth)

waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
-- waitFor (Mgr mgr) tid = do
--   maybeDone <- modifyMVar mgr $ \m ->
--     return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
--       (Nothing, _) -> (m, Nothing)
--       (done, m') -> (m', done)
--   case maybeDone of
--     Nothing -> return Nothing
--     Just st -> Just `fmap` takeMVar st
waitFor (Mgr mgr) tid =
  join . modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, return Nothing)
      (Just st, m') -> (m', Just `fmap` takeMVar st)

waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
    where elems m = return (M.empty, M.elems m)
