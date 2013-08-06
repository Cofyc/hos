import GHC.Conc
import Control.Concurrent
import HoS.HoS.System.Environment
import Control.Concurrent.Chan
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad
import Data.Char
import Data.List
import HoS.Network.HTTP
import Control.Applicative
import HoS.Network.Socket
import HoS.Network.BSD
import HoS.HoS.System.IO
import Data.Word
import HoS.Network.CGI hiding (urlEncode)
import HoS.Network.CGI.Protocol hiding (urlEncode)
import HoS.Network.URI (unEscapeString, escapeURIString, isUnescapedInURI)
import HoS.Network.HTTP.Base (urlEncode, urlDecode, urlEncodeVars)
import Control.Exception
import HoS.Network.Handler.FastCGI

main = 
    getDataFrom "/asfasf" >>=
    print

--main = do
--    rs <- try $ myThrow :: (Exception e) => IO (Either e ())
--    print rs

myThrow = do
    throw $ PatternMatchFail "asf"
