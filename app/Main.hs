{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Conduit.Network
    ( appSink, appSource, serverSettings, runTCPServer )
import Data.Conduit ( (.|), runConduit )
import qualified Data.Conduit.Combinators as C ( map )
import RespParseConduit (respParse)


main :: IO ()
main = runTCPServer (serverSettings 6379 "*") (\ app ->
    runConduit $ appSource app .|
      respParse  .| 
      C.map (const "-ERR Not implemented\r\n") .|
      appSink app
  )
