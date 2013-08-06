module HoS.System.Thread (
    ThreadManager,
    newManager,
    forkManaged,
    waitFor,
    waitForAll
) where

import Control.Concurrent
import Control.Exception (SomeException, try)
import Control.Monad (join)
import Data.Maybe
import qualified Data.Map as M

data ThreadStatus = Running
                  | Finished                -- terminated normally
                  | Threw SomeException     -- killed by uncaught exception
                    deriving (Show)

newtype ThreadManager = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
    deriving (Eq)

-- | Create a new thread manager.
newManager :: IO ThreadManager
newManager = Mgr `fmap` newMVar M.empty

-- | Create a new managed thread.
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
    modifyMVar mgr $ \m -> do
        state <- newEmptyMVar
        threadId <- forkIO $ do
            result <- try body
            putMVar state (either Threw (const Finished) result)
        return (M.insert threadId state m, threadId)

-- | Block until a specific managed thread terminates.
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid =
    join . modifyMVar mgr $
        \m -> return $
            case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
                (Nothing, _) -> (m, return Nothing)
                (Just status, m') -> (m', Just `fmap` takeMVar status)

-- | Block until all managed threads terminate.
waitForAll :: ThreadManager -> IO ()
waitForAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
    where
        elems m = return (M.empty, M.elems m)
