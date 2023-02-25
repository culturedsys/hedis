{-# LANGUAGE OverloadedStrings #-}
module CommandProcessorSpec(spec) where
import Test.Hspec (Spec, describe, it, shouldReturn)
import AppState (newState)
import CommandProcessor (handleCommand)
import Command (Command(..), SetArgs(..), GetArgs(..), IncrArgs(..))
import Control.Concurrent.STM (atomically)
import Resp (Resp(..))

spec :: Spec
spec = do
  describe "CommandProcess.handleCommand" $ do
    it "should handle a set and a get command" $ do
      appState <- atomically newState
      atomically (handleCommand appState (Set $ SetArgs "key" "value")) `shouldReturn` SimpleString "OK"
      atomically (handleCommand appState (Get $ GetArgs "key")) `shouldReturn` BulkString "value"

    it "should handle an incr command" $ do
      appState <- atomically newState
      atomically (handleCommand appState (Incr $ IncrArgs "key")) `shouldReturn` Integer 1

    it "should handle a setnx command" $ do
      appState <- atomically newState
      atomically (handleCommand appState (SetNx $ SetArgs "key" "value")) `shouldReturn` SimpleString "OK"
      atomically (handleCommand appState (SetNx $ SetArgs "key" "value")) `shouldReturn` NullString
