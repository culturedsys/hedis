{-# LANGUAGE OverloadedStrings #-}

module CommandProcessorSpec (spec) where

import Command (Command (..), DecrArgs (..), GetArgs (..), IncrArgs (..), SetArgs (..))
import CommandProcessor (handleCommand)
import Control.Concurrent.STM (atomically, newTVarIO)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Resp (Resp (..))
import Store qualified
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  let now = UTCTime (fromGregorian 1970 1 1) 0
  describe "CommandProcess.handleCommand" $ do
    it "should handle a set and a get command" $ do
      store <- newTVarIO Store.empty
      atomically (handleCommand (Set $ SetArgs "key" "value") now store) `shouldReturn` SimpleString "OK"
      atomically (handleCommand (Get $ GetArgs "key") now store) `shouldReturn` BulkString "value"

    it "should handle an incr command" $ do
      store <- newTVarIO Store.empty
      atomically (handleCommand (Incr $ IncrArgs "key") now store) `shouldReturn` Integer 1

    it "should handle a decr command" $ do
      store <- newTVarIO Store.empty
      atomically (handleCommand (Decr $ DecrArgs "key") now store) `shouldReturn` Integer (-1)

    it "should handle a setnx command" $ do
      store <- newTVarIO Store.empty
      atomically (handleCommand (SetNx $ SetArgs "key" "value") now store) `shouldReturn` SimpleString "OK"
      atomically (handleCommand (SetNx $ SetArgs "key" "value") now store) `shouldReturn` NullString
