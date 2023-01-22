{-# LANGUAGE OverloadedStrings #-}
module CommandProcessorSpec(spec) where
import Test.Hspec (Spec, describe, it, shouldReturn)
import AppState (newState, AppState(..))
import CommandProcessor (handleCommand)
import Command (Command(..))
import Control.Concurrent.STM (atomically, readTVarIO, newTVar)
import qualified Data.Map as M
import Resp (Resp(..))

spec :: Spec
spec = do
  describe "CommandProcess.handleCommand" $ do
    it "should handle a set command" $ do
      appState <- atomically newState
      atomically (handleCommand appState (Set "key" "value")) `shouldReturn` SimpleString "OK"
      readTVarIO (store appState) `shouldReturn` M.fromList [("key", "value")]
    it "should handle a get command" $ do
      appState <- atomically $ AppState <$> newTVar (M.fromList [("key", "value")])
      atomically (handleCommand appState (Get "key")) `shouldReturn` BulkString "value"