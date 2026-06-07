module PMS.Infra.Agent.Server.Interface.Network where

import Network.Socket

import PMS.Infra.Agent.Server.CoreModel.Type

-- | Default ServerPortFunc using real network operations.
defaultServerPortFunc :: ServerPortFunc
defaultServerPortFunc = ServerPortFunc
  { _bindListenServerPortFunc  = networkBindListen
  , _acceptConnServerPortFunc  = networkAcceptConn
  , _closeSocketServerPortFunc = networkCloseSocket
  , _closeConnServerPortFunc   = networkCloseConn
  }

-- | Bind and listen on the given host and port.
-- Returns the listening Socket on success; throws IO exception on failure.
networkBindListen :: String -> String -> IO Socket
networkBindListen host port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  sock   <- socket (addrFamily addr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 1
  return sock

-- | Accept one connection on the listening socket.
-- Returns a (Socket, remote-address) pair.
networkAcceptConn :: Socket -> IO (Socket, String)
networkAcceptConn listenSock = do
  (connSock, peerAddr) <- accept listenSock
  return (connSock, show peerAddr)

-- | Close the listening socket.
networkCloseSocket :: Socket -> IO ()
networkCloseSocket = close

-- | Close the accepted connection socket.
-- Use immediate close here because agent-server-close must not wait for a peer
-- or for a recvLoop blocked in network I/O.
networkCloseConn :: Socket -> IO ()
networkCloseConn = close
