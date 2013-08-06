module HoS.Core (
    module HoS.Network.Base,
    module HoS.Network.Server,
    module HoS.System.Thread,
    module HoS.Network.Http,
    hosServe,
) where

import Control.Concurrent
import Control.Exception
import Data.Char
import Data.DateTime
import Data.List
import Data.Maybe
import HoS.Network.Base
import HoS.Network.Handler
import HoS.Network.Http
import HoS.Network.Server
import HoS.Network.Util
import HoS.System.Thread
import Network.Socket hiding (KeepAlive)
import System.Directory
import System.FilePath
import System.IO
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types

-- | Start Listening Socket for svHost which have same sockets binded on same port!
startListeningSockets :: ThreadManager -> [[SvHost]] -> IO [ThreadId]
startListeningSockets mgr svHostsGroup = mapM (startListeningSocket mgr) svHostsGroup
    where
        startListeningSocket :: ThreadManager -> [SvHost] -> IO ThreadId
        startListeningSocket mgr svHosts = do
            forkManaged mgr $ waitRequests svHosts (svSock $ svHosts !! 0)
        -- | Listening incoming connection requests
        waitRequests :: [SvHost] -> Socket -> IO ()
        waitRequests svHosts sock = do
            (connsock, clientaddr) <- accept sock
            connhdl <- socketToHandle connsock ReadWriteMode
            forkIO $ handleRequest svHosts connhdl clientaddr `finally` hClose connhdl
            waitRequests svHosts sock

-- | Handle each request
handleRequest :: [SvHost] -> Handle -> SockAddr -> IO ()
handleRequest hosts h clientaddr = hGetContents h >>=
    parseRequest >>=
    tempRequestToResponse hosts >>= 
    hPutStr h . show
--logHandler (svLogFd host) clientaddr request response

tempRequestToResponse :: [SvHost] -> (Either SomeException Request) -> IO Response
tempRequestToResponse hosts (Left _) = getDefault400Response
tempRequestToResponse hosts (Right request) =  
                case getRightHost hosts request of
                    Just host ->
                        dispatchHandler host request
                    Nothing -> getDefault400Response

dispatchHandler :: SvHost -> Request -> IO Response
dispatchHandler host request 
    | ext == "php" = getResponseFromCGI host request
    | otherwise = getResponseWithRequest host request
    where
        ext = map toLower $ drop 1 $ takeExtension (rqURI request)

getResponseWithRequest :: SvHost -> Request -> IO Response
getResponseWithRequest host request = do
    now <- getCurrentTime
    let requestUri = rqURI request
    let requestScriptName = takeWhile (/= '?') requestUri
    let rootpath = svRoot $ svConfig host
    let filepath = rootpath ++ requestScriptName
    fileInfo <- getFileInfoWithIndices ["index.html"] filepath
    case fileInfo of
        Just (contents, mimeType, len, lastMod) -> do
            case (getRequestHeader request If_Modified_Since) of
                Just timeStr -> do
                    if (fromHttpDate timeStr >= lastMod)
                        then return $ getResponse 1.1 304
                                    [ (Date, httpDate now)
                                    , (Server, "hos")
                                    , (LastModified, httpDate lastMod)
                                    , (ContentType, mimeType)
                                    , (ContentLength, show len)
                                    ] ""
                        else return $ getResponse 1.1 200
                                    [ (Date, httpDate now)
                                    , (Server, "hos")
                                    , (LastModified, httpDate lastMod)
                                    , (ContentType, mimeType)
                                    , (ContentLength, show len)
                                    ] contents
                Nothing -> return $ getResponse 1.1 200
                            [ (Date, httpDate now)
                            , (Server, "hos")
                            , (LastModified, httpDate lastMod)
                            , (ContentType, mimeType)
                            , (ContentLength, show len)
                            ] contents
        Nothing -> getDefault403Response

-- | Bad Request Response
getDefault400Response :: IO Response
getDefault400Response = do
    now <- getCurrentTime
    let contents = "<html><head><title>400 Bad Request</title></head><body><center><h1>400 Bad Request</h1><h2>Your client sent a request that this server could not understand.</h2></center><hr><center>hos</center></body></html>"
    return $ getResponse 1.1 400
        [ (Server, "hos")
        , (Date, httpDate now)
        , (ContentType, "text/html")
        , (ContentLength, show $ length contents)
        ] contents
getDefault404Response :: IO Response
getDefault404Response = do
    now <- getCurrentTime
    let contents = "<html><head><title>404 Not Found</title></head><body><center><h1>404 Not Found</h1></center><hr><center>hos</center></body></html>"
    return $ getResponse 1.1 404
        [ (Server, "hos")
        , (Date, httpDate now)
        , (ContentType, "text/html")
        , (ContentLength, show $ length contents)
        ] contents
getDefault403Response :: IO Response
getDefault403Response = do
    now <- getCurrentTime
    let contents = "<html><head><title>403 Forbidden</title></head><body><center><h1>403 Forbidden</h1></center><hr><center>hos</center></body></html>"
    return $ getResponse 1.1 403
        [ (Server, "hos")
        , (Date, httpDate now)
        , (ContentType, "text/html")
        , (ContentLength, show $ length contents)
        ] contents
getDefault301Response :: String -> IO Response
getDefault301Response location = do
    now <- getCurrentTime
    let contents = "<html><head><title>301 Moved Permanently</title></head><body><center><h1>301 Moved Permanently</h1></center><hr><center>hos</center></body></html>"
    return $ getResponse 1.1 301
        [ (Server, "hos")
        , (Date, httpDate now)
        , (ContentType, "text/html")
        , (ContentLength, show $ length contents)
        ] contents


-- | get File Information With Given Path And Default Indices
getFileInfo :: FilePath -> IO (String, String, Int, DateTime)
getFileInfo filepath = do
    handle <- openFile filepath ReadMode
    lastModTime <- getModificationTime filepath
    len <- hFileSize handle
    contents <- hGetContents handle
    return $
        ( contents
        , getMimeType filepath
        , fromInteger len
        , fromClockTime lastModTime
        )

getFileInfoWithIndices :: [String] -> FilePath -> IO (Maybe (String, String, Int, DateTime))
getFileInfoWithIndices [] filepath = fmap Just $ getFileInfo filepath
getFileInfoWithIndices (index:indices) filepath = do
    isFile <- doesFileExist filepath
    isDir <- doesDirectoryExist filepath
    if isFile
        then fmap Just $ getFileInfo filepath
        else if isDir
                then do
                    let newFilePath = filepath ++ index
                    isFile' <- doesFileExist newFilePath
                    if isFile'
                        then fmap Just $ getFileInfo newFilePath
                        else getFileInfoWithIndices indices filepath
                else do
                    return Nothing


getRightHost :: [SvHost] -> Request -> Maybe SvHost
getRightHost [] _ = Nothing
getRightHost (svHost:svHosts) request = if isSameHost then Just svHost else getRightHost svHosts request
    where
        hostStr = fromJust $ getRequestHeader request Host
        isSameHost =
            if hostStr == (svHostName $ svConfig svHost)
                then True
            else if hostStr == (svHostName $ svConfig svHost) ++ ":" ++ show (svPort $ svConfig svHost)
                then True
            else False

-- A simple handler that prints incoming packets
logHandler :: Fd -> SockAddr -> Request -> Response -> IO ()
logHandler fd addr request response = do
    now <- getCurrentTime
    let (SockAddrInet _ hostAddr) = addr
    hostAddrStr <- inet_ntoa hostAddr
    _ <- fdWrite fd $  "[" ++ httpDate now ++ "]" ++ 
        " " ++ hostAddrStr ++ 
        " " ++ show (rqMethod request) ++ 
        " " ++ show (rqProtocol request) ++
        " " ++ show (rsStatus response) ++
        " " ++ (rqURI request) ++ "\n"
    return ()

hosServe :: HsConfig -> [SvConfig] -> IO ()
hosServe hsConfig svCfgs = do
    pid <- getProcessID
    fd_pid <- openFd "/var/run/hos.pid" WriteOnly (Just 0o644) defaultFileFlags
    let filelock = (WriteLock, AbsoluteSeek, 0, 0)
    setLock fd_pid filelock
    fdWrite fd_pid (show pid)
    svHosts <- initSvHosts hsConfig svCfgs
    mgr <- newManager
--    closeFd stdInput
--    closeFd stdOutput
--    closeFd stdError
    startListeningSockets mgr (groupHostsBySock svHosts)
    waitForAll mgr
