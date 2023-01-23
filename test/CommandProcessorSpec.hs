{-# LANGUAGE OverloadedStrings #-}
module CommandProcessorSpec(spec) where
import Test.Hspec (Spec, describe, it, shouldReturn)
import AppState (newState)
import CommandProcessor (handleCommand)
import Command (Command(..))
import Control.Concurrent.STM (atomically)
import Resp (Resp(..))

spec :: Spec
spec = do
  describe "CommandProcess.handleCommand" $ do
    it "should handle a set and a get command" $ do
      appState <- atomically newState
      atomically (handleCommand appState (Set "key" "value")) `shouldReturn` SimpleString "OK"
      atomically (handleCommand appState (Get "key")) `shouldReturn` BulkString "value"