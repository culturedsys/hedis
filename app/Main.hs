{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Conduit.Network
    ( appSink, appSource, serverSettings, runTCPServer )
import Data.Conduit ( (.|), runConduit )
import qualified Data.Conduit.Combinators as C ( mapM )
import Control.Concurrent.STM (atomically)
import RespParseConduit (respParse, respWrite)
import CommandProcessor (processCommand)
import AppState (newState)

main :: IO ()
main = do
  appState <- atomically newState
  runTCPServer (serverSettings 6379 "*") (\ app ->
    runConduit $ appSource app .|
      respParse  .| 
      C.mapM (processCommand appState) .|
      respWrite .|
      appSink app
    )
