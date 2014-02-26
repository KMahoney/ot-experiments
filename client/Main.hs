{-# LANGUAGE OverloadedStrings #-}

import Network.Simple.TCP
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent
import Control.Concurrent.MVar
import Data.List (splitAt)


data Op = Insert Int String
        deriving (Show, Read)

type State = String

-- head is most recent
type History = [(State, Op)]

apply str (Insert i ins) = left ++ ins ++ right
  where (left, right) = splitAt i str

-- First argument has priority, returns modified second argument
merge :: Op -> Op -> Op
merge (Insert n1 s1) (Insert n2 s2) = Insert n2' s2
  where n2' | n2 >= n1 = n2 + length s1
            | otherwise = n2

currentState :: History -> State
currentState [] = ""
currentState ((state, op) : _) = apply state op

-- `resolve` assumes the operation already in the history has priority
-- (i.e. the order we resolve events is the order we received them)

-- The next step would be adding precedence information to the history
-- so operations can be applied locally first and then correctly
-- merged after receiving operations from the server. Alternatively,
-- this implementation is good enough for merging server-side and
-- broadcasting a complete state to clients.
resolve :: (State, Op) -> History -> History
resolve act@(state, op) history = (currentState history, op') : history
  where mergeList = takeWhile (\(hstate, hop) -> apply hstate hop /= state) history
        op' = foldr merge op $ map snd mergeList

-- Receive operations from the server and apply them to the current state
receiveLoop socket historyVar = do
  let maxRecv = 64000
  r <- recv socket maxRecv
  case r of
    Just msg -> do
      let (opState, op) = (read $ BS.unpack msg) :: (String, Op)
      putStrLn $ "Received: " ++ show op ++ " for " ++ opState
      history <- takeMVar historyVar
      let history' = resolve (opState, op) history
      putStrLn $ "State: " ++ currentState history'
      putMVar historyVar history'
      receiveLoop socket historyVar
    Nothing -> putStrLn "Connection lost"

-- Read operations from stdin and send them to the server
inputLoop socket historyVar = do
  command <- getLine
  case command of
    "q" -> return ()
    _ -> case reads command of
      [] -> do
        putStrLn "Invalid Operation"
        inputLoop socket historyVar
      ((op, _) : _) -> do
        let op' = op :: Op -- keep the type checker happy with ambiguous `reads`
        putStrLn $ "Operation: " ++ show op
        history <- readMVar historyVar
        send socket $ BS.pack $ show (currentState history, op)
        inputLoop socket historyVar

handler (socket, addr) = do
  historyVar <- newMVar []
  receiveThread <- forkIO $ receiveLoop socket historyVar
  inputLoop socket historyVar
  killThread receiveThread

main = connect "localhost" "8000" handler
