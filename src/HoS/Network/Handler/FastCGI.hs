module HoS.Network.Handler.FastCGI (
    getResponseFromCGI,
    getDataFrom,
) where

import HoS.Network.Base
import HoS.Network.Server
import HoS.System.Thread
import HoS.Network.Http
import HoS.Network.Util

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Internal as BSI
import Network.Socket
import Data.Word
import Data.DateTime
import Data.Maybe
import Data.Char
import Data.Bits
import qualified Data.Map as M

data FastCGIRecord = FastCGIRecord
    { fcgiVersion :: Word8
    , fcgiType :: Word8
    , fcgiRequestIdB1 :: Word8
    , fcgiRequestIdB0 :: Word8
    , fcgiContentLengthB1 :: Word8
    , fcgiContentLengthB0 :: Word8
    , fcgiPaddingLength :: Word8
    , fcgiReserved :: Word8
    , fcgiContentData :: [Word8]
    , fcgiPaddingData :: [Word8]
    } deriving (Show)

--recordToString :: FastCGIRecord -> String
--recordToString record = map BSI.w2c $

--writeRecord :: Socket -> FastCGIRecord -> IO Bool 
--writeRecord sock record = do

getResponseFromCGI :: SvHost -> Request -> IO Response
getResponseFromCGI host request = do
    now <- getCurrentTime
    let filepath = svRoot (svConfig host) ++ rqURI request
    contents <- getDataFrom filepath
    let contents' = reverse . (dropWhile (=='\0')) . (drop 16) . reverse $ contents
    let (headers, body) = fromJust $ splitAtEmptyLine contents'
    return $ getResponse 1.1 200
        [ (Server, "hos")
        , (Date, httpDate now)
--        , (ContentType, fromJust $ getRequestHeader req ContentType)
        , (ContentLength, show $ length body)
        ] body

getDataFrom :: String -> IO String
getDataFrom filepath = do
    hostAddress <- inet_addr "127.0.0.1"
    let sockAddr = SockAddrInet 9000 hostAddress
    -- Create a socket
    sock <- socket AF_INET Stream defaultProtocol
    -- Connect to server
    connect sock sockAddr
    let fisrt = [ '\1', '\1', '\0', '\0', '\0', '\8', '\0', '\0'
                , '\0', '\1', '\0', '\0', '\0', '\0', '\0', '\0'
                ]
    let nameParis =
            [ ("GATEWAY_INTERFACE", "FastCGI/1.0")
            , ("REQUEST_METHOD", "GET")
            , ("SCRIPT_FILENAME", filepath)
            , ("SERVER_PORT", "8080")
            ]
    let params = toNameValuePairs nameParis
    let params_len =  length params
    let params_padding_len = (8 - params_len `mod` 8)
    let second = ['\1', '\4', '\0', '\1'] ++ showContentLength params_len ++ showPaddingLength params_padding_len ++ ['\0']
    let second' = params ++ replicate params_padding_len '\0'
    let third = ['\1', '\4', '\0', '\1', '\0', '\0', '\0', '\0']
    let four = ['\1', '\5', '\0', '\1', '\0', '\0', '\0', '\0']
    let request = [fisrt, second, second', third, four]
    print $ map length request
    send sock $ concat request
    s <- parseFastCGIRecord sock
    sClose sock
    return s

showContentLength :: Int -> String
showContentLength num = map chr . map (.&. 0xff) $ [shiftR num 8, num]

showPaddingLength :: Int -> String
showPaddingLength num = map chr . map (.&. 0xff) $ [num]

toNameValuePairs :: [ (String, String) ] -> String
toNameValuePairs (x:xs) = toNameValuePair x ++ toNameValuePairs xs
    where
        toNameValuePair (name, value) = 
                (showLength . length $ name) ++ (showLength . length $ value)
                ++ name ++ value
            where
                showLength :: Int -> String
                showLength num 
                    | num <= 127 = [chr num]
                    | otherwise = map chr . map (.&. 0xff) $
                        [ clearBit (shiftR num 24) 7
                        , shiftR num 16
                        , shiftR num 8
                        , num 
                        ]

toNameValuePairs [] = ""

parseFastCGIRecord :: Socket -> IO String
parseFastCGIRecord sock = do
    s <- recv sock 8
    s <- recv sock 100000
    return s
