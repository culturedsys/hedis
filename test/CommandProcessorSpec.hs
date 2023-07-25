{-# LANGUAGE OverloadedStrings #-}
module CommandProcessorSpec(spec) where
import Test.Hspec (Spec, describe, it, shouldReturn)
import CommandProcessor (handleCommand)
import Command (Command(..), SetArgs(..), GetArgs(..), IncrArgs(..), DecrArgs(..))
import Control.Concurrent.STM (atomically, newTVarIO)
import Resp (Resp(..))
import Data.Time (fromGregorian, UTCTime (UTCTime))
import qualified Store

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
