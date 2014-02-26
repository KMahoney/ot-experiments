import Network.Simple.TCP
import Control.Concurrent
import Control.Concurrent.Chan
import Data.ByteString (ByteString)

maxRecv = 64000

receiveLoop :: Chan ByteString -> Socket -> IO ()
receiveLoop chan socket = do
  r <- recv socket maxRecv
  case r of
    Just msg -> writeChan chan msg >> receiveLoop chan socket
    Nothing -> return ()

broadcastLoop :: Chan ByteString -> Socket -> IO ()
broadcastLoop chan socket = do
  msg <- readChan chan
  threadDelay 10000000 -- simulate latency
  send socket msg
  broadcastLoop chan socket

handler :: Chan ByteString -> (Socket, SockAddr) -> IO ()
handler chan (socket, addr) = do
  putStrLn $ "Connection " ++ show addr
  chan' <- dupChan chan
  broadcastThread <- forkIO $ broadcastLoop chan' socket
  receiveLoop chan socket
  killThread broadcastThread

logChan :: Chan ByteString -> IO ()
logChan chan = do
  msg <- readChan chan
  putStrLn $ "Received: " ++ show msg
  logChan chan

main = do
  chan <- newChan
  forkIO $ logChan chan
  serve (Host "127.0.0.1") "8000" (handler chan)
