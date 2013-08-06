module HoS.Network.Base (
    HsConfig (..),
    SvConfig (..),
    SvHost (..),
    UserAndGroup (..),
    setUserAndGroup,
    groupHostsBySock,
    withSvHost,
) where

import Control.Monad.State
import Network.Socket
import Data.Word
import Data.List
import System.Posix.User
import System.Posix.Types

-- | Global Host Server Configure
data HsConfig = HsConfig
    { maxKeepAliveRequests :: Int, -- ^ The maximum number of requests to allow during a persistent connect.
      configFile :: FilePath,
      logFile :: FilePath
    } deriving (Show)

-- | Server Config
data SvConfig = SvConfig
    { svName :: String
    , svHostName :: String
    , svPort :: Word16 -- Port Number
    , svRoot :: FilePath
    } deriving (Show, Read)

-- | Server Host, it contains all infomation of one host instance
data SvHost = SvHost
    { hsConfig :: HsConfig
    , svConfig :: SvConfig
    , svSock :: Socket
    , svLogFd :: Fd
    } deriving (Show)

type SvHostStateT a = StateT SvHost IO a

withSvHost :: SvHost -> SvHostStateT a -> IO (a, SvHost)
withSvHost svHost svHostStateT = svHostStateT `runStateT` svHost

-- | User&Group Data Type
data  UserAndGroup = UserAndGroup String String | UserWithDefaultGroup String

-- | Set the user and group for the process. If the group is Nothing, then use the users default group.
-- This is especially useful when you are root and want to become a user.
setUserAndGroup :: UserAndGroup -> IO ()
setUserAndGroup (UserWithDefaultGroup username) = do
    e <- getUserEntryForName username
    setGroupID (userGroupID e)
    setUserID (userID e)
setUserAndGroup (UserAndGroup username groupname) = do
    e <- getUserEntryForName username
    ge <- getGroupEntryForName groupname
    if username `elem` groupMembers ge
        then setGroupID (userGroupID e) >> setUserID (userID e)
        else error ("user " ++ username ++ " is not in group " ++ groupname)

-- | Group hosts by port
groupHostsBySock :: [SvHost] -> [[SvHost]]
groupHostsBySock svHosts = groupBy (\a b -> svSock a == svSock b) svHostsSorted
    where
        svHostsSorted = sortBy (\a b -> svPort (svConfig a) `compare` svPort (svConfig b)) svHosts
