{-# LANGUAGE OverloadedStrings #-}

import Web.SocketIO

main = server 4000 $ do
  on "connected" $ reply >>= broadcast "connected"
  on "chat" $ do
    msg <- reply
    emit "chat" msg
    broadcast "chat" msg
  on "ping" $ emit "pong" []
