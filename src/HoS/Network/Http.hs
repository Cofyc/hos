{-# OPTIONS_GHC -XBangPatterns #-}

module HoS.Network.Http (
    Request(..),
    Response(..),
    parseRequest,
    getResponse,
    setResponseHeaders,
    ResponseHeader(..),
    HttpProtocol(..),
    HttpStatus(..),
    getMimeType,
    RequestMethod (..),
    getRequestHeader,
    RequestHeader (..),
) where

import HoS.Network.Util
import Data.Maybe
import Data.List
import System.FilePath
import qualified Data.Map as M
import Control.Exception

-- | Http Message
--data HttpMessage = Request | Response

-- | Http Version
type HttpVersion = Float

-- | Http Protocol
data HttpProtocol = Http HttpVersion
instance Show HttpProtocol where
    show (Http version) = "HTTP/" ++ show version
instance Read HttpProtocol where
    -- readsPrec is the main function for parsing input
    readsPrec _ value =
        -- We pass tryParse a list of pairs. Each pair has a string
        -- and the desired return value. tryParse will try to match
        -- the input to one of these strings.
        tryParse [("HTTP/1.0", Http 1.0), ("HTTP/1.1", Http 1.1)]
        where
            tryParse [] = []
            tryParse ((attempt, result):xs) =
                    if  (take (length attempt) value) == attempt
                        then [(result, drop (length attempt) value)]
                        else tryParse xs

-- | Http Request Method
data RequestMethod = GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT deriving (Show, Read)

-- | Http Request URI
type RequestURI = String

-- | Http Request Header
data RequestHeader = Host
                   | UserAgent
                   | Accept
                   | AcceptLanguage
                   | AcceptEncoding
                   | Cookie
                   | If_Modified_Since
                   | Range
                   deriving (Read, Eq)
instance Show RequestHeader where
    show Host = "Host"
    show UserAgent = "User-Agent"
    show Accept = "Accept"
    show AcceptLanguage = "Accept-Language"
    show AcceptEncoding = "Accept-Encoding"
    show Cookie = "Cookie"
    show If_Modified_Since = "If-Modified-Since"
    show Range = "Range"

-- | Http Status Code
type HttpStatusCode = Int

-- | Http Response Header
data ResponseHeader = Server
                    | Date
                    | ContentType
                    | ContentLength
                    | LastModified
                    | Connection
                    | KeepAlive
                    | Etag
instance Show ResponseHeader where
    show Server = "Server"
    show Date = "Date"
    show ContentType = "Content-Type"
    show ContentLength = "Content-Length"
    show LastModified = "Last-Modified"
    show Connection = "Connection"
    show KeepAlive = "Keep-Alive"
    show Etag = "Etag"

-- | Http Message Header
data HeaderField = ResponseHeaderField (ResponseHeader, String)
                 | RequestHeaderField (RequestHeader, String)
instance Show HeaderField where
    show (ResponseHeaderField (responseHeader, string)) = show responseHeader ++ ": " ++ string ++ "\r\n"
    show (RequestHeaderField (requestHeader, string)) = show requestHeader ++ ": " ++ string ++ "\r\n"

-- | Http Request
data Request = Request
    { rqMethod :: RequestMethod -- request status Line
    , rqURI :: RequestURI
    , rqProtocol :: HttpProtocol
    , rqHeaders :: [HeaderField] -- header fields (general, request, entity)
    , rqBody :: String -- message body
    }
instance Show Request where
    show (Request method uri protocol headers body)
        = show method ++ " " ++ uri ++ show protocol ++ "\r\n"
        ++ concat (map show headers)
        ++ "\r\n"
        ++ body

-- | Http Response
data Response = Response
    { rsProtocol :: HttpProtocol
    , rsStatus :: HttpStatus
    , rsHeaders :: [HeaderField] -- header fields (general, request, entity)
    , rsBody :: String -- message body
    }
instance Show Response where
    show (Response protocl status headers body)
        = show protocl ++ " " ++ show status ++ "\r\n"
        ++ concat (map show headers)
        ++ "\r\n"
        ++ body

-- | getResponse
getResponse :: HttpVersion -> HttpStatusCode -> [(ResponseHeader, String)] -> String -> Response
getResponse version code responseHeaderPairs body = Response
    { rsProtocol =  Http version
    , rsStatus = HttpStatus code
    , rsHeaders = map ResponseHeaderField responseHeaderPairs
    , rsBody = body
    }

-- | setResponseHeaders
setResponseHeaders :: Response -> [(ResponseHeader, String)] -> Response
setResponseHeaders response responseHeaderPairs = Response
    { rsProtocol =  rsProtocol response
    , rsStatus = rsStatus response
    , rsHeaders = (rsHeaders response) ++ (map ResponseHeaderField responseHeaderPairs)
    , rsBody = rsBody response
    }

-- | get Header String
getRequestHeader :: Request -> RequestHeader -> Maybe String
getRequestHeader request rqHeader = getRequestHeader' (rqHeaders request) rqHeader
    where
        getRequestHeader' :: [HeaderField] -> RequestHeader -> Maybe String
        getRequestHeader' (RequestHeaderField (hder, str):hds) header =
            if hder == header then Just str else getRequestHeader' hds header
        getRequestHeader' _ _ = Nothing

-- | Request Line Parser
parseRequestLine :: String -> Maybe (RequestMethod, RequestURI, HttpProtocol)
parseRequestLine str = do
    if (length ws) == 3
        then Just
            ( read (ws!!0) :: RequestMethod
            , ws!!1
            , read (ws!!2) :: HttpProtocol
            )
        else Nothing
    where
        ws = words str

-- | Request Headers Parser
parseRequestHeaders :: [HeaderField] -> String -> [HeaderField]
parseRequestHeaders headers str =
    case (parseRequestHeader str) of
        Just header -> header : headers
        Nothing -> headers
    where
        parseRequestHeader :: String -> Maybe HeaderField
        parseRequestHeader str =
            case header' of
                Just header -> Just $ RequestHeaderField (header, string)
                Nothing -> Nothing
            where
                header' = readRequestHeader $ trimStr $ takeWhile (/=':') str
                string = trimStr . (drop 1) $ dropWhile (/=':') str
        readRequestHeader :: String -> Maybe RequestHeader
        readRequestHeader str
            | str == "Host" = Just Host
            | str == "User-Agent" = Just UserAgent
            | str == "Accept" = Just Accept
            | str == "AcceptLanguage" = Just AcceptLanguage
            | str == "Cookie" = Just Cookie
            | str == "If-Modified-Since" = Just If_Modified_Since
            | str == "Range" = Just Range
            | otherwise = Nothing

-- | parse Requst
parseRequest :: String -> IO (Either SomeException Request)
parseRequest str = try $
    case splitAtEmptyLine str of
        Just (headerString, bodyString) -> do
                let ls = linesCRLF headerString
                case parseRequestLine (head ls) of
                    -- using bang pattern to strict these, before return
                    Just (!method, !uri, !protocol) ->
                        return Request
                            { rqMethod = method
                            , rqURI = uri
                            , rqProtocol = protocol
                            , rqHeaders = foldl parseRequestHeaders [] (drop 1 ls)
                            , rqBody = bodyString
                            }
                    Nothing -> throwIO $ PatternMatchFail "Parsing Request Line Failed."
        Nothing -> throwIO $ PatternMatchFail "No empty line."

-- | Http Status
data HttpStatus = HttpStatus HttpStatusCode
instance Show HttpStatus where
    show (HttpStatus 100) = "100 Continue"
    show (HttpStatus 101) = "101 Switching Protocols"
    show (HttpStatus 200) = "200 OK"
    show (HttpStatus 201) = "201 Created"
    show (HttpStatus 202) = "202 Accepted"
    show (HttpStatus 203) = "203 Non-Authoritative Information"
    show (HttpStatus 204) = "204 No Content"
    show (HttpStatus 205) = "205 Reset Content"
    show (HttpStatus 206) = "206 Partial Content"
    show (HttpStatus 300) = "300 Multiple Choices"
    show (HttpStatus 301) = "301 Moved Permanently"
    show (HttpStatus 302) = "302 Found"
    show (HttpStatus 303) = "303 See Other"
    show (HttpStatus 304) = "304 Not Modified"
    show (HttpStatus 305) = "305 Use Proxy"
    show (HttpStatus 307) = "307 Temporary Redirect"
    show (HttpStatus 400) = "400 Bad Request"
    show (HttpStatus 401) = "401 Unauthorized"
    show (HttpStatus 402) = "402 Payment Required"
    show (HttpStatus 403) = "403 Forbidden"
    show (HttpStatus 404) = "404 Not Found"
    show (HttpStatus 405) = "405 Method Not Allowed"
    show (HttpStatus 406) = "406 Not Acceptable"
    show (HttpStatus 407) = "407 Proxy Authentication Required"
    show (HttpStatus 408) = "408 Request Time-out"
    show (HttpStatus 409) = "409 Conflict"
    show (HttpStatus 410) = "410 Gone"
    show (HttpStatus 411) = "411 Length Required"
    show (HttpStatus 412) = "412 Precondition Failed"
    show (HttpStatus 413) = "413 Request Entity Too Large"
    show (HttpStatus 414) = "414 Request-URI Too Large"
    show (HttpStatus 415) = "415 Unsupported Media Type"
    show (HttpStatus 416) = "416 Requested range not satisfiable"
    show (HttpStatus 417) = "417 Expectation Failed"
    show (HttpStatus 500) = "500 Internal Server Error"
    show (HttpStatus 501) = "501 Not Implemented"
    show (HttpStatus 502) = "502 Bad Gateway"
    show (HttpStatus 503) = "503 Service Unavailable"
    show (HttpStatus 504) = "504 Gateway Time-out"
    show (HttpStatus 505) = "505 HTTP Version not supported"
    show (HttpStatus code) = show code ++ " Not Specified Status Code"

-- | Mime Type
type MimeType = String

getMimeType :: FilePath -> MimeType
getMimeType filepath = M.findWithDefault defaultMimeType ext mimeTypes
    where
        ext = drop 1 $ takeExtension filepath -- get extension without dot

defaultMimeType :: MimeType
defaultMimeType = "application/octet-stream" 

mimeTypes :: M.Map String String
mimeTypes = M.fromList
    [ ("html", "text/html")
    , ("htm", "text/html")
    , ("css", "text/css")
    , ("xml", "text/xml")
    , ("gif", "image/gif")
    , ("jpg", "image/jpeg")
    , ("jpeg", "image/jpeg")
    , ("js", "application/x-javascript")
    , ("atom", "application/atom+xml")
    , ("rss", "application/rss+xml")
    , ("mml", "text/mathml")
    , ("txt", "text/plain")
    , ("jad", "text/vnd.sun.j2me.app-descriptor")
    , ("wml", "text/vnd.wap.wml")
    , ("htc", "text/x-component")
    , ("png", "image/png")
    , ("tiff", "image/tiff")
    , ("tif", "image/tiff")
    , ("wbmp", "image/vnd.wap.wbmp")
    , ("ico", "image/x-icon")
    , ("jng", "image/x-jng")
    , ("bmp", "image/x-ms-bmp")
    , ("svg", "image/svg+xml")
    , ("jar", "application/java-archive")
    , ("ear", "application/java-archive")
    , ("war", "application/java-archive")
    , ("hqx", "application/mac-binhex40")
    , ("doc", "application/msword")
    , ("pdf", "application/pdf")
    , ("ps", "application/postscript")
    , ("ai", "application/postscript")
    , ("eps", "application/postscript")
    , ("rtf", "application/rtf")
    , ("xls", "application/vnd.ms-excel")
    , ("ppt", "application/vnd.ms-powerpoint")
    , ("wmlc", "application/vnd.wap.wmlc")
    , ("xhtml", "application/vnd.wap.xhtml+xml")
    , ("kml", "application/vnd.google-earth.kml+xml")
    , ("kmz", "application/vnd.google-earth.kmz")
    , ("cco", "application/x-cocoa")
    , ("jardiff", "application/x-java-archive-diff")
    , ("jnlp", "application/x-java-jnlp-file")
    , ("run", "application/x-makeself")
    , ("pm", "application/x-perl")
    , ("pl", "application/x-perl")
    , ("pdb", "application/x-pilot")
    , ("prc", "application/x-pilot")
    , ("rar", "application/x-rar-compressed")
    , ("rpm", "application/x-redhat-package-manager")
    , ("sea", "application/x-sea")
    , ("swf", "application/x-shockwave-flash")
    , ("sit", "application/x-stuffit")
    , ("tk", "application/x-tcl")
    , ("tcl", "application/x-tcl")
    , ("der", "application/x-x509-ca-cert")
    , ("crt", "application/x-x509-ca-cert")
    , ("pem", "application/x-x509-ca-cert")
    , ("xpi", "application/x-xpinstall")
    , ("zip", "application/zip")
    , ("bin", "application/octet-stream")
    , ("dll", "application/octet-stream")
    , ("exe", "application/octet-stream")
    , ("deb", "application/octet-stream")
    , ("dmg", "application/octet-stream")
    , ("eot", "application/octet-stream")
    , ("img", "application/octet-stream")
    , ("iso", "application/octet-stream")
    , ("msi", "application/octet-stream")
    , ("msm", "application/octet-stream")
    , ("msp", "application/octet-stream")
    , ("mid", "audio/midi")
    , ("midi", "audio/midi")
    , ("kar", "audio/midi")
    , ("mp3", "audio/mpeg")
    , ("ra", "audio/x-realaudio")
    , ("3gp", "video/3gpp")
    , ("3gpp", "video/3gpp")
    , ("mpg", "video/mpeg")
    , ("mpeg", "video/mpeg")
    , ("mov", "video/quicktime")
    , ("flv", "video/x-flv")
    , ("mng", "video/x-mng")
    , ("asf", "video/x-ms-asf")
    , ("asx", "video/x-ms-asf")
    , ("wmv", "video/x-ms-wmv")
    , ("avi", "video/x-msvideo")
    ]
