module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Net.Server as Serv
import Node.Net.Socket as Sock
import Node.Encoding as Encode
import Data.Options as Opt
import Data.Unit
import Data.Either as E
import Data.Maybe as M
import Node.Buffer as Buff
import Effect.Exception as Except

main :: Effect Unit
main = do
 server <- Serv.createServer (Opt.Options []) connectionCallback
 a      <- Serv.listenTCP server 8989 "10.0.0.248" 2 listenCallback 
 pure unit

connectionCallback:: Sock.Socket -> Effect Unit
connectionCallback s = do
    addr <- Sock.remoteAddress s
    port <- Sock.remotePort s
    _    <- Sock.setEncoding s Encode.UTF8
    _    <- Sock.onData s handleData
    _    <- Sock.onClose s $ handleClose addr
    _    <- Sock.onError s handleError 
    logShow addr
    logShow port
    pure unit

listenCallback :: Effect Unit
listenCallback = do
    log "hi"
    pure unit

handleData :: E.Either Buff.Buffer String -> Effect Unit
handleData (E.Left b) = pure unit
handleData (E.Right s) = do
    log s
    pure unit

handleClose :: M.Maybe String -> Boolean -> Effect Unit
handleClose addr false = do
    log $ "closed!" <> (show addr)
    pure unit
handleClose _ _ = pure unit

handleError :: Except.Error -> Effect Unit
handleError e = do
    logShow e
    pure unit

