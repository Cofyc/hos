import System.Console.GetOpt
import System.Environment (getProgName, getArgs)
import System.Posix.Process (forkProcess)
import System.Posix.Types (ProcessID)
import Foreign.Marshal.Error (void)
import qualified Control.Exception as CE
import qualified System.IO.Error as SIOE
import HoS.Core

data Opt = Version | Help | ConfigFile FilePath | LogFile FilePath | Debug
    deriving (Show, Eq)

options :: [OptDescr Opt]
options =
    [ Option ['v'] ["version"] (NoArg Version) "show version info"
    , Option ['h'] ["help"] (NoArg Help) "show usage help"
    , Option ['d'] ["debug"] (NoArg Debug) "debug mode"
    , Option ['c'] ["config-file"] (ReqArg ConfigFile "config file") "configuration file path"
    , Option ['l'] ["log-file"] (ReqArg LogFile "log file") "log file path"
    ]

compileOpts :: [String] -> IO ([Opt], [String])
compileOpts args =
    case getOpt Permute options args of
        (o, n, []) -> return (o, n)
        (_, _, errs) -> 
            getProgName >>=
            CE.ioError . SIOE.userError . errorFormat
            where
                errorFormat progName = ""
                    ++ concat (map ("hos: "++) errs)
                    ++ "\n"
                    ++ usageInfo ("Usage: " ++ progName ++ " [OPTION]") options
                    ++ "\n"

-- | Top Exception Handlers
topIOExceptionHandler :: CE.IOException -> IO ()
topIOExceptionHandler e = putStr $ SIOE.ioeGetErrorString e
topArithExceptionHandler :: CE.ArithException -> IO ()
topArithExceptionHandler e = putStr $ show e
topSomeExceptionHandler :: CE.SomeException -> IO ()
topSomeExceptionHandler e = do
    putStr $ "Unhandled exceptions: "
        ++ "\n"
        ++ "Exception: " ++ show e
        ++ "\n"

-- | Main
main :: IO ()
main = CE.catches main1 topExceptionHandlers
    where
        topExceptionHandlers :: [CE.Handler ()]
        topExceptionHandlers =
            [ CE.Handler topIOExceptionHandler
            , CE.Handler topArithExceptionHandler
            , CE.Handler topSomeExceptionHandler
            ]

main1 :: IO ()
main1 = do
    args <- getArgs
    (opts, args) <- compileOpts args
    let hsConfig = HsConfig 250 "/etc/hos.conf" "/var/log/hos.log"
    configStr <- readFile (configFile hsConfig)
    let svConfigs = read configStr :: [SvConfig]
    if Debug `elem` opts
        then do
            hosServe hsConfig svConfigs
        else do
            void $ forkProcess (hosServe hsConfig svConfigs)
