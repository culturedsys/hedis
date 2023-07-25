{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AppState (newState)
import CommandProcessor (processCommand)
import Control.Concurrent.STM (atomically)
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Combinators qualified as C (mapM)
import Data.Conduit.Network (appSink, appSource, runTCPServer, serverSettings)
import RespParseConduit (respParse, respWrite)

main :: IO ()
main = do
  appState <- atomically newState
  runTCPServer
    (serverSettings 6379 "*")
    ( \app ->
        runConduit $
          appSource app
            .| respParse
            .| C.mapM (processCommand appState)
            .| respWrite
            .| appSink app
    )
