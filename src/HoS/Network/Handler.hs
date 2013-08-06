module HoS.Network.Handler (
    module HoS.Network.Handler.Default,
    module HoS.Network.Handler.FastCGI
) where


import HoS.Network.Base
import HoS.Network.Server
import HoS.System.Thread
import HoS.Network.Http
import HoS.Network.Util
import HoS.Network.Handler.Default
import HoS.Network.Handler.FastCGI

--requestHandler :: SvHost
