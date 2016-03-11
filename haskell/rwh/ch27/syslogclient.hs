
-- file: ch27/syslogclient.hs
import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import SyslogTypes


data SyslogHandle = 
    SyslogHandle {slSocket :: Socket,
                  slProgram :: String,
                  slAddress :: SockAddr}

openlog :: HostName             -- ^ Remote hostname, or localhost
        -> String               -- ^ Port number or name; 514 is default
        -> String               -- ^ Name to log under
        -> IO SyslogHandle      -- ^ Handle to use for logging

openlog hostname port progname =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos
      
       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Save off the socket, program name, and server address in a handle
       return $ SyslogHandle sock progname (addrAddress serveraddr)


syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogh fac pri msg =
    sendstr sendmsg
    where code = makeCode fac pri
          sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++
                    ": " ++ msg
          -- Send until everything is done
          sendstr :: String -> IO ()
          sendstr [] = return ()
          sendstr omsg = do sent <- sendTo (slSocket syslogh) omsg
                                    (slAddress syslogh)
                            sendstr (genericDrop sent omsg)


closelog :: SyslogHandle -> IO ()
closelog syslogh = sClose (slSocket syslogh)


{- | Convert a facility and a priority into a syslog code -}
makeCode :: Facility -> Priority -> Int
makeCode fac pri =
    let faccode = codeOfFac fac
        pricode = fromEnum pri 
        in
          (faccode `shiftL` 3) .|. pricode

          
