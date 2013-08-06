module HoS.Network.Server (
    initSvHosts
) where

import HoS.Network.Base
import Control.Monad.State
import Network.Socket
import Network.BSD
import System.IO
import Data.List
import Data.Word
import System.Posix.IO
import System.Posix.Types

-- | make an IP Address: (127,0,0,1) is the localhost
ipAddress :: (Word8, Word8, Word8, Word8) -> HostAddress
ipAddress (a, b, c, d) = fromIntegral a + 0x100 * fromIntegral b + 0x10000 * fromIntegral c + 0x1000000 * fromIntegral d

-- | get Server Sockets
getServerSocks :: HsConfig -> [Word16] -> IO [(Word16, Socket)]
getServerSocks hsConfig [] = return []
getServerSocks hsConfig (port:ports) = do
    -- SockAddr (using ipv4 wildcard address: INADDR_ANY)
    let sockAddr = SockAddrInet (fromIntegral port) iNADDR_ANY
    -- Create a socket
    sock <- socket AF_INET Stream defaultProtocol
    -- Set socket options
    setSocketOption sock ReuseAddr 1
    -- Bind it to the address we're listening to
    bindSocket sock sockAddr
    -- Start listening for connection requests, and set its maximum queue size as follow
    listen sock (maxKeepAliveRequests hsConfig)
    -- Recursive Merging
    liftM2 (:) (return (port, sock)) (getServerSocks hsConfig ports)

initSvHosts :: HsConfig -> [SvConfig] -> IO [SvHost]
initSvHosts hsConfig svConfigs = do
    let ports = nub $ map svPort svConfigs
    serverSocks <- getServerSocks hsConfig ports
    fd_log <- openFd (logFile hsConfig) WriteOnly (Just 0o644) logFileFlags
    return $ map (initSvHost hsConfig serverSocks fd_log) svConfigs
    where
        initSvHost :: HsConfig -> [(Word16, Socket)] -> Fd -> SvConfig -> SvHost
        initSvHost hsConfig socketInfos fd svConfig = SvHost hsConfig svConfig sock fd
            where (Just sock) = lookup (svPort svConfig) socketInfos

logFileFlags :: OpenFileFlags
logFileFlags = OpenFileFlags
    { append = True
    , exclusive = False
    , noctty = False
    , nonBlock = False
    , trunc = False
    }
